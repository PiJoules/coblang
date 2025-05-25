#include <gtest/gtest.h>
#include <libcob.h>
#include <string>
#include <new>

#include "coblang.h"

// These are unittests for functions part of the cobl-* library.
//
// Invoke with `COB_LIBRARY_PATH=build/ COB_PRE_LOAD=<program-id> ...
// to ensure other entry points are callable.
//
// Example:
//  
//   COB_LIBRARY_PATH=build COB_PRE_LOAD=cobl-string ./build/test-coblang
//
// The `run-tests.sh` script invokes this test with the right env variables.

namespace {

class CoblTest : public testing::Test {
 protected:
  void SetUp() override {
    cob_init(0, NULL);
  }
};

//
// Helper functions
//
void string_construct(CoblString *str) {
  void *cob_argv[] = {str};
  ASSERT_EQ(cob_call("string-construct", 1, cob_argv), 0);
}

void string_construct_from_c_str(CoblString *str, const char *cstr) {
  void *cob_argv[] = {str, (void *)(&cstr)};
  ASSERT_EQ(cob_call("string-construct-from-c-str", 2, cob_argv), 0);
}

unsigned long string_length(CoblString *str) {
  unsigned long size;
  void *cob_argv[] = {str, &size};
  cob_call("string-length", 2, cob_argv);
  return size;
}

char *string_data(CoblString *str) {
  char *cobl_str_data = nullptr;
  void *cob_argv[] = {str, &cobl_str_data};
  cob_call("string-data", 2, cob_argv);
  return cobl_str_data;
}

void string_push_back(CoblString *str, char c) {
  void *cob_argv[] = {str, &c};
  ASSERT_EQ(cob_call("string-push-back", 2, cob_argv), 0);
}

void string_destroy(CoblString *str) {
  void *cob_argv[] = {str};
  ASSERT_EQ(cob_call("string-destroy", 1, cob_argv), 0);
}

int string_compare_c_string(CoblString *str, const char *s) {
  int ret;
  void *cob_argv[] = {str, &s, &ret};
  cob_call("string-compare-c-string", 3, cob_argv);
  return ret;
}

void string_clear(CoblString *str) {
  void *cob_argv[] = {str};
  cob_call("string-clear", 1, cob_argv);
}

void string_erase(CoblString *str, unsigned long idx) {
  void *cob_argv[] = {str, &idx};
  cob_call("string-erase", 2, cob_argv);
}

char string_at(CoblString *str, unsigned long idx) {
  char c;
  void *cob_argv[] = {str, &idx, &c};
  cob_call("string-at", 3, cob_argv);
  return c;
}

TEST_F(CoblTest, NewString) {
  CoblString cobl_str;
  string_construct(&cobl_str);

  EXPECT_EQ(string_length(&cobl_str), 0);

  string_destroy(&cobl_str);
}

TEST_F(CoblTest, MultipleStrings) {
  constexpr const char *kOriginal[] = {"abc", "1234", "", "12\0ab"};
  constexpr size_t kNumStrs = sizeof(kOriginal) / sizeof(*kOriginal);
  CoblString strs[kNumStrs];

  for (size_t i = 0; i < kNumStrs; ++i) {
    string_construct_from_c_str(&strs[i], kOriginal[i]);
  }

  for (size_t i = 0; i < kNumStrs; ++i) {
    EXPECT_EQ(string_length(&strs[i]), strlen(kOriginal[i]));
  }

  for (size_t i = 0; i < kNumStrs; ++i) {
    char *cobl_str_data = string_data(&strs[i]);
    EXPECT_NE(cobl_str_data, nullptr);
    EXPECT_STREQ(kOriginal[i], cobl_str_data);
  }

  for (size_t i = 0; i < kNumStrs; ++i) {
    string_destroy(&strs[i]);
  }
}

TEST_F(CoblTest, StringCompare) {
  CoblString s;
  string_construct_from_c_str(&s, "abc");

  EXPECT_EQ(string_compare_c_string(&s, "abc"), 0);
  EXPECT_GT(string_compare_c_string(&s, "abb"), 0);
  EXPECT_GT(string_compare_c_string(&s, "Abc"), 0);
  EXPECT_LT(string_compare_c_string(&s, "abd"), 0);
  EXPECT_EQ(string_compare_c_string(&s, "abc\0a"), 0);

  string_destroy(&s);

  string_construct_from_c_str(&s, "Abc");

  EXPECT_LT(string_compare_c_string(&s, "abc"), 0);

  string_destroy(&s);

  string_construct_from_c_str(&s, "123");

  EXPECT_EQ(string_compare_c_string(&s, "123"), 0);
  EXPECT_LT(string_compare_c_string(&s, "1234"), 0);
  EXPECT_LT(string_compare_c_string(&s, "abc"), 0);

  string_destroy(&s);
}

TEST_F(CoblTest, StringClear) {
  CoblString s;
  string_construct_from_c_str(&s, "abc");

  string_clear(&s);
  EXPECT_EQ(string_compare_c_string(&s, ""), 0);
  EXPECT_EQ(string_length(&s), 0);
  EXPECT_STREQ(string_data(&s), "");

  string_destroy(&s);
}

TEST_F(CoblTest, StringAt) {
  CoblString s;
  string_construct_from_c_str(&s, "abc");

  EXPECT_EQ(string_at(&s, 0), 'a');
  EXPECT_EQ(string_at(&s, 1), 'b');
  EXPECT_EQ(string_at(&s, 2), 'c');

  string_destroy(&s);
}

TEST_F(CoblTest, StringErase) {
  CoblString s;
  string_construct_from_c_str(&s, "abc");

  string_erase(&s, 1);
  EXPECT_EQ(string_compare_c_string(&s, "ac"), 0);
  EXPECT_EQ(string_length(&s), 2);

  string_erase(&s, 1);
  EXPECT_EQ(string_compare_c_string(&s, "a"), 0);
  EXPECT_EQ(string_length(&s), 1);

  string_erase(&s, 0);
  EXPECT_EQ(string_compare_c_string(&s, ""), 0);
  EXPECT_EQ(string_length(&s), 0);

  string_destroy(&s);
}

TEST_F(CoblTest, StringPushBack) {
  constexpr size_t kSizes[] = {1, 10, 100, 1000, 10000, 100000};
  constexpr char kChar = '=';

  for (size_t size : kSizes) {
    const std::string kOriginal(size, kChar);
    CoblString str;
    string_construct(&str);

    for (size_t i = 0; i < size; ++i) {
      string_push_back(&str, kChar);
    }

    EXPECT_EQ(string_length(&str), size);
    EXPECT_STREQ(string_data(&str), kOriginal.data());

    string_destroy(&str);
  }
}

void vector_construct(CoblVector *v, unsigned long elem_size,
                      unsigned long elem_align) {
  void *cob_argv[] = {v, &elem_size, &elem_align};
  ASSERT_EQ(cob_call("vector-construct", 3, cob_argv), 0);
}

void vector_destroy(CoblVector *v) {
  void *cob_argv[] = {v};
  ASSERT_EQ(cob_call("vector-destroy", 1, cob_argv), 0);
}

unsigned long vector_size(CoblVector *v) {
  unsigned long size;
  void *cob_argv[] = {v, &size};
  cob_call("vector-size", 2, cob_argv);
  return size;
}

unsigned long vector_capacity(CoblVector *v) {
  unsigned long cap;
  void *cob_argv[] = {v, &cap};
  cob_call("vector-capacity", 2, cob_argv);
  return cap;
}

void *vector_data(CoblVector *v) {
  void *data;
  void *cob_argv[] = {v, &data};
  cob_call("vector-data", 2, cob_argv);
  return data;
}

unsigned long vector_elem_size(CoblVector *v) {
  unsigned long size;
  void *cob_argv[] = {v, &size};
  cob_call("vector-elem-size", 2, cob_argv);
  return size;
}

unsigned long vector_elem_align(CoblVector *v) {
  unsigned long align;
  void *cob_argv[] = {v, &align};
  cob_call("vector-elem-align", 2, cob_argv);
  return align;
}

void vector_reserve(CoblVector *v, unsigned long newcap) {
  void *cob_argv[] = {v, &newcap};
  ASSERT_EQ(cob_call("vector-reserve", 2, cob_argv), 0);
}

void *vector_append_storage(CoblVector *v) {
  void *ptr;
  void *cob_argv[] = {v, &ptr};
  cob_call("vector-append-storage", 2, cob_argv);
  return ptr;
}

void *vector_at(CoblVector *v, unsigned long i) {
  void *ptr;
  void *cob_argv[] = {v, &i, &ptr};
  cob_call("vector-at", 3, cob_argv);
  return ptr;
}

TEST_F(CoblTest, NewVector) {
  CoblVector v;
  vector_construct(&v, sizeof(int), alignof(int));

  EXPECT_EQ(vector_size(&v), 0);
  EXPECT_NE(vector_data(&v), nullptr);
  EXPECT_EQ(vector_elem_size(&v), sizeof(int));
  EXPECT_EQ(vector_elem_align(&v), alignof(int));
  EXPECT_GT(vector_capacity(&v), 0);

  vector_destroy(&v);
}

TEST_F(CoblTest, VectorReserve) {
  CoblVector v;
  vector_construct(&v, sizeof(int), alignof(int));

  unsigned long cap = vector_capacity(&v);
  vector_reserve(&v, cap * 2);
  EXPECT_EQ(vector_capacity(&v), cap * 2);

  vector_destroy(&v);
}

TEST_F(CoblTest, VectorAppendStorage) {
  CoblVector v;
  vector_construct(&v, sizeof(int), alignof(int));

  ptrdiff_t diff;

  for (size_t i = 0; i < 10; ++i) {
    void *storage = vector_append_storage(&v);
    ASSERT_EQ(((uintptr_t)storage) % alignof(int), 0);
    ASSERT_EQ(vector_size(&v), i + 1);
    *(int *)storage = i * 10;
    ASSERT_EQ(*(int *)vector_at(&v, i), i * 10);

    if (i > 0) {
      uintptr_t last = (uintptr_t)vector_at(&v, i - 1);
      if (i > 1) {
        ASSERT_EQ((uintptr_t)storage - last, diff);
      } else {
        diff = (uintptr_t)storage - last;
      }
    }
  }

  vector_destroy(&v);
}

void tree_map_construct(CoblTreeMap *m, TreeMapCmp *cmp, KeyCtor *ctor,
                        KeyDtor *dtor) {
  void *cob_argv[] = {m, &cmp, &ctor, &dtor};
  cob_call("tree-map-construct", 4, cob_argv);
}

void string_tree_map_construct(CoblTreeMap *m) {
  void *cob_argv[] = {m};
  cob_call("string-tree-map-construct", 1, cob_argv);
}

void tree_map_destroy(CoblTreeMap *m) {
  void *cob_argv[] = {m};
  cob_call("tree-map-destroy", 1, cob_argv);
}

void *tree_map_left(CoblTreeMap *m) {
  void *ret;
  void *cob_argv[] = {m, &ret};
  cob_call("tree-map-left", 2, cob_argv);
  return ret;
}

void *tree_map_right(CoblTreeMap *m) {
  void *ret;
  void *cob_argv[] = {m, &ret};
  cob_call("tree-map-right", 2, cob_argv);
  return ret;
}

void *tree_map_key(CoblTreeMap *m) {
  void *ret;
  void *cob_argv[] = {m, &ret};
  cob_call("tree-map-key", 2, cob_argv);
  return ret;
}

void *tree_map_value(CoblTreeMap *m) {
  void *ret;
  void *cob_argv[] = {m, &ret};
  cob_call("tree-map-value", 2, cob_argv);
  return ret;
}

void tree_map_set(CoblTreeMap *m, const void *key, const void *val) {
  void *cob_argv[] = {m, &key, &val};
  cob_call("tree-map-set", 3, cob_argv);
}

bool tree_map_get(CoblTreeMap *m, const void *key, void **val) {
  char ret;
  void *cob_argv[] = {m, &key, val, &ret};
  cob_call("tree-map-get", 4, cob_argv);
  return ret == 'Y';
}

void tree_map_clear(CoblTreeMap *m) {
  void *cob_argv[] = {m};
  cob_call("tree-map-clear", 1, cob_argv);
}

void tree_map_clone(CoblTreeMap *dst, CoblTreeMap *src) {
  void *cob_argv[] = {dst, src};
  cob_call("tree-map-clone", 2, cob_argv);
}

void tree_map_erase(CoblTreeMap *m, const void *key) {
  char ret;
  void *cob_argv[] = {m, &key};
  cob_call("tree-map-erase", 2, cob_argv);
}

TEST_F(CoblTest, TreeMapConstruction) {
  CoblTreeMap m;
  tree_map_construct(&m, nullptr, nullptr, nullptr);
  ASSERT_EQ(tree_map_left(&m), nullptr);
  ASSERT_EQ(tree_map_right(&m), nullptr);
  ASSERT_EQ(tree_map_key(&m), nullptr);
  ASSERT_EQ(tree_map_value(&m), nullptr);
  tree_map_destroy(&m);
}

TEST_F(CoblTest, StringTreeMapConstruction) {
  CoblTreeMap m;
  string_tree_map_construct(&m);
  ASSERT_EQ(tree_map_left(&m), nullptr);
  ASSERT_EQ(tree_map_right(&m), nullptr);
  ASSERT_EQ(tree_map_key(&m), nullptr);
  ASSERT_EQ(tree_map_value(&m), nullptr);
  tree_map_destroy(&m);
}

TEST_F(CoblTest, TreeMapInsertion) {
  CoblTreeMap m;
  string_tree_map_construct(&m);

  void *res;
  ASSERT_FALSE(tree_map_get(&m, "key", &res));

  const char *val = "val";
  tree_map_set(&m, "key", val);

  ASSERT_TRUE(tree_map_get(&m, "key", &res));
  ASSERT_EQ(res, val);

  const char *val2 = "val2";
  tree_map_set(&m, "key2", val2);

  ASSERT_TRUE(tree_map_get(&m, "key2", &res));
  ASSERT_EQ(res, val2);
  ASSERT_TRUE(tree_map_get(&m, "key", &res));
  ASSERT_EQ(res, val);
  ASSERT_NE(tree_map_right(&m), nullptr);
  ASSERT_EQ(tree_map_left(&m), nullptr);

  tree_map_destroy(&m);
}

TEST_F(CoblTest, TreeMapOverrideKeyValue) {
  CoblTreeMap m;
  string_tree_map_construct(&m);

  const char *val = "val";
  tree_map_set(&m, "key", val);

  void *res;
  ASSERT_TRUE(tree_map_get(&m, "key", &res));
  ASSERT_EQ(res, val);

  const char *newval = "newval";
  tree_map_set(&m, "key", newval);

  ASSERT_TRUE(tree_map_get(&m, "key", &res));
  ASSERT_EQ(res, newval);

  tree_map_destroy(&m);
}

TEST_F(CoblTest, TreeMapClone) {
  CoblTreeMap m;
  string_tree_map_construct(&m);

  const char *val = "val";
  tree_map_set(&m, "key", val);

  tree_map_clear(&m);
  ASSERT_EQ(tree_map_left(&m), nullptr);
  ASSERT_EQ(tree_map_right(&m), nullptr);
  ASSERT_EQ(tree_map_key(&m), nullptr);

  CoblTreeMap m2;
  string_tree_map_construct(&m2);
  tree_map_set(&m2, "key", val);
  const char *val2 = "val2";
  tree_map_set(&m2, "key2", (char *)val2);

  tree_map_clone(&m, &m2);
  ASSERT_STREQ((const char *)tree_map_key(&m), "key");
  ASSERT_STREQ((const char *)tree_map_key(&m2), "key");

  void *res;
  ASSERT_TRUE(tree_map_get(&m, "key", &res));
  ASSERT_STREQ((const char *)res, val);
  ASSERT_TRUE(tree_map_get(&m2, "key", &res));
  ASSERT_STREQ((const char *)res, val);
  ASSERT_TRUE(tree_map_get(&m, "key2", &res));
  ASSERT_STREQ((const char *)res, val2);
  ASSERT_TRUE(tree_map_get(&m2, "key2", &res));
  ASSERT_STREQ((const char *)res, val2);

  tree_map_destroy(&m);
  tree_map_destroy(&m2);
}

TEST_F(CoblTest, TreeMapCloneEmpty) {
  CoblTreeMap m, m2;
  string_tree_map_construct(&m);
  string_tree_map_construct(&m2);

  tree_map_clone(&m, &m2);
  ASSERT_EQ(tree_map_key(&m), nullptr);
  ASSERT_EQ(tree_map_key(&m2), nullptr);

  tree_map_destroy(&m);
  tree_map_destroy(&m2);
}

TEST_F(CoblTest, TreeMapErase) {
  CoblTreeMap m;
  string_tree_map_construct(&m);

  const char *val = "val";
  const char *key = "key";
  tree_map_set(&m, key, val);
  ASSERT_STREQ((const char *)tree_map_key(&m), key);

  tree_map_erase(&m, key);
  ASSERT_EQ(tree_map_key(&m), nullptr);

  tree_map_destroy(&m);
}

TEST_F(CoblTest, TreeMapErase2) {
  CoblTreeMap m;
  string_tree_map_construct(&m);

  const char *val = "val";
  const char *key = "key";
  tree_map_set(&m, key, val);

  const char *val2 = "val2";
  const char *key2 = "key2";
  tree_map_set(&m, key2, val2);

  void *res;
  ASSERT_TRUE(tree_map_get(&m, key2, &res));
  ASSERT_EQ(res, val2);
  ASSERT_TRUE(tree_map_get(&m, key, &res));
  ASSERT_EQ(res, val);

  ASSERT_NE(tree_map_right(&m), nullptr);
  ASSERT_EQ(tree_map_left(&m), nullptr);

  auto *m2 = std::launder((CoblTreeMap *)tree_map_right(&m));
  ASSERT_EQ(tree_map_right(m2), nullptr);
  ASSERT_EQ(tree_map_left(m2), nullptr);

  tree_map_erase(&m, key);
  ASSERT_STREQ((const char *)tree_map_key(&m), key2);
  ASSERT_EQ(tree_map_right(&m), nullptr);
  ASSERT_EQ(tree_map_left(&m), nullptr);
  ASSERT_TRUE(tree_map_get(&m, key2, &res));
  ASSERT_EQ(res, val2);

  tree_map_destroy(&m);
}

}  // namespace
