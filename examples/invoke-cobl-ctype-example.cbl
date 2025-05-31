       IDENTIFICATION DIVISION.
         PROGRAM-ID. invoke-cobl-memcpy-example.
       DATA DIVISION.
         local-storage section.
           01 res pic x.

       procedure division.
         call "cobl-isspace" using res " ".
         display "1) " res.
         call "cobl-isspace" using res "a".
         display "2) " res.
         call "cobl-isspace" using res "1".
         display "3) " res.
         call "cobl-isdigit" using res " ".
         display "4) " res.
         call "cobl-isdigit" using res "a".
         display "5) " res.
         call "cobl-isdigit" using res "1".
         display "6) " res.
