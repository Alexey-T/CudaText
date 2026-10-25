      *> Sample COBOL source for lexer benchmarking
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCHMARK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MAX-ITEMS PIC 9(4) VALUE 168.
       01 WS-TOTAL PIC S9(9) VALUE ZERO.
       01 WS-NAME PIC X(16) VALUE 'bench-'.
       01 WS-I PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAX-ITEMS
               COMPUTE WS-TOTAL = WS-TOTAL + WS-I * 9
           END-PERFORM

       PARA_3702.
           DISPLAY 'STEP PARA_3702 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_3405.
           DISPLAY 'STEP PARA_3405 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_8112.
           DISPLAY 'STEP PARA_8112 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_6726.
           DISPLAY 'STEP PARA_6726 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_6724.
           DISPLAY 'STEP PARA_6724 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_4015.
           DISPLAY 'STEP PARA_4015 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       STOP RUN.

      *> Sample COBOL source for lexer benchmarking
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCHMARK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MAX-ITEMS PIC 9(4) VALUE 755.
       01 WS-TOTAL PIC S9(9) VALUE ZERO.
       01 WS-NAME PIC X(16) VALUE 'bench-'.
       01 WS-I PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAX-ITEMS
               COMPUTE WS-TOTAL = WS-TOTAL + WS-I * 9
           END-PERFORM

       PARA_8788.
           DISPLAY 'STEP PARA_8788 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_5499.
           DISPLAY 'STEP PARA_5499 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_889.
           DISPLAY 'STEP PARA_889 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_218.
           DISPLAY 'STEP PARA_218 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_4453.
           DISPLAY 'STEP PARA_4453 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_1899.
           DISPLAY 'STEP PARA_1899 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       STOP RUN.

      *> Sample COBOL source for lexer benchmarking
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCHMARK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MAX-ITEMS PIC 9(4) VALUE 102.
       01 WS-TOTAL PIC S9(9) VALUE ZERO.
       01 WS-NAME PIC X(16) VALUE 'bench-'.
       01 WS-I PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAX-ITEMS
               COMPUTE WS-TOTAL = WS-TOTAL + WS-I * 7
           END-PERFORM

       PARA_6946.
           DISPLAY 'STEP PARA_6946 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_6610.
           DISPLAY 'STEP PARA_6610 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_7992.
           DISPLAY 'STEP PARA_7992 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_7877.
           DISPLAY 'STEP PARA_7877 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_4483.
           DISPLAY 'STEP PARA_4483 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_1867.
           DISPLAY 'STEP PARA_1867 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       STOP RUN.

      *> Sample COBOL source for lexer benchmarking
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCHMARK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MAX-ITEMS PIC 9(4) VALUE 450.
       01 WS-TOTAL PIC S9(9) VALUE ZERO.
       01 WS-NAME PIC X(16) VALUE 'bench-'.
       01 WS-I PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAX-ITEMS
               COMPUTE WS-TOTAL = WS-TOTAL + WS-I * 2
           END-PERFORM

       PARA_1432.
           DISPLAY 'STEP PARA_1432 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_6136.
           DISPLAY 'STEP PARA_6136 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_5189.
           DISPLAY 'STEP PARA_5189 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_4622.
           DISPLAY 'STEP PARA_4622 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_8993.
           DISPLAY 'STEP PARA_8993 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_482.
           DISPLAY 'STEP PARA_482 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       STOP RUN.

      *> Sample COBOL source for lexer benchmarking
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCHMARK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MAX-ITEMS PIC 9(4) VALUE 485.
       01 WS-TOTAL PIC S9(9) VALUE ZERO.
       01 WS-NAME PIC X(16) VALUE 'bench-'.
       01 WS-I PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAX-ITEMS
               COMPUTE WS-TOTAL = WS-TOTAL + WS-I * 3
           END-PERFORM

       PARA_795.
           DISPLAY 'STEP PARA_795 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_2638.
           DISPLAY 'STEP PARA_2638 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_3148.
           DISPLAY 'STEP PARA_3148 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_7292.
           DISPLAY 'STEP PARA_7292 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_6231.
           DISPLAY 'STEP PARA_6231 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_9469.
           DISPLAY 'STEP PARA_9469 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       STOP RUN.

      *> Sample COBOL source for lexer benchmarking
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCHMARK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MAX-ITEMS PIC 9(4) VALUE 584.
       01 WS-TOTAL PIC S9(9) VALUE ZERO.
       01 WS-NAME PIC X(16) VALUE 'bench-'.
       01 WS-I PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAX-ITEMS
               COMPUTE WS-TOTAL = WS-TOTAL + WS-I * 4
           END-PERFORM

       PARA_3267.
           DISPLAY 'STEP PARA_3267 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_4776.
           DISPLAY 'STEP PARA_4776 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_4928.
           DISPLAY 'STEP PARA_4928 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_20.
           DISPLAY 'STEP PARA_20 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_1759.
           DISPLAY 'STEP PARA_1759 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_2647.
           DISPLAY 'STEP PARA_2647 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       STOP RUN.

      *> Sample COBOL source for lexer benchmarking
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCHMARK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MAX-ITEMS PIC 9(4) VALUE 863.
       01 WS-TOTAL PIC S9(9) VALUE ZERO.
       01 WS-NAME PIC X(16) VALUE 'bench-'.
       01 WS-I PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAX-ITEMS
               COMPUTE WS-TOTAL = WS-TOTAL + WS-I * 6
           END-PERFORM

       PARA_2507.
           DISPLAY 'STEP PARA_2507 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_7840.
           DISPLAY 'STEP PARA_7840 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_5464.
           DISPLAY 'STEP PARA_5464 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_181.
           DISPLAY 'STEP PARA_181 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_2102.
           DISPLAY 'STEP PARA_2102 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_7261.
           DISPLAY 'STEP PARA_7261 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       STOP RUN.

      *> Sample COBOL source for lexer benchmarking
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCHMARK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MAX-ITEMS PIC 9(4) VALUE 175.
       01 WS-TOTAL PIC S9(9) VALUE ZERO.
       01 WS-NAME PIC X(16) VALUE 'bench-'.
       01 WS-I PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAX-ITEMS
               COMPUTE WS-TOTAL = WS-TOTAL + WS-I * 4
           END-PERFORM

       PARA_3256.
           DISPLAY 'STEP PARA_3256 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_2690.
           DISPLAY 'STEP PARA_2690 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_4719.
           DISPLAY 'STEP PARA_4719 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_9355.
           DISPLAY 'STEP PARA_9355 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_9847.
           DISPLAY 'STEP PARA_9847 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_7662.
           DISPLAY 'STEP PARA_7662 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       STOP RUN.

      *> Sample COBOL source for lexer benchmarking
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCHMARK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MAX-ITEMS PIC 9(4) VALUE 55.
       01 WS-TOTAL PIC S9(9) VALUE ZERO.
       01 WS-NAME PIC X(16) VALUE 'bench-'.
       01 WS-I PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAX-ITEMS
               COMPUTE WS-TOTAL = WS-TOTAL + WS-I * 7
           END-PERFORM

       PARA_3022.
           DISPLAY 'STEP PARA_3022 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_3144.
           DISPLAY 'STEP PARA_3144 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_5714.
           DISPLAY 'STEP PARA_5714 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_8625.
           DISPLAY 'STEP PARA_8625 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_9457.
           DISPLAY 'STEP PARA_9457 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_4673.
           DISPLAY 'STEP PARA_4673 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       STOP RUN.

      *> Sample COBOL source for lexer benchmarking
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCHMARK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MAX-ITEMS PIC 9(4) VALUE 582.
       01 WS-TOTAL PIC S9(9) VALUE ZERO.
       01 WS-NAME PIC X(16) VALUE 'bench-'.
       01 WS-I PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAX-ITEMS
               COMPUTE WS-TOTAL = WS-TOTAL + WS-I * 7
           END-PERFORM

       PARA_9150.
           DISPLAY 'STEP PARA_9150 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_6923.
           DISPLAY 'STEP PARA_6923 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_8489.
           DISPLAY 'STEP PARA_8489 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_9152.
           DISPLAY 'STEP PARA_9152 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_1719.
           DISPLAY 'STEP PARA_1719 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_3167.
           DISPLAY 'STEP PARA_3167 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       STOP RUN.

      *> Sample COBOL source for lexer benchmarking
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCHMARK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MAX-ITEMS PIC 9(4) VALUE 996.
       01 WS-TOTAL PIC S9(9) VALUE ZERO.
       01 WS-NAME PIC X(16) VALUE 'bench-'.
       01 WS-I PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAX-ITEMS
               COMPUTE WS-TOTAL = WS-TOTAL + WS-I * 4
           END-PERFORM

       PARA_3995.
           DISPLAY 'STEP PARA_3995 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_8808.
           DISPLAY 'STEP PARA_8808 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_9043.
           DISPLAY 'STEP PARA_9043 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_8191.
           DISPLAY 'STEP PARA_8191 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_3080.
           DISPLAY 'STEP PARA_3080 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_328.
           DISPLAY 'STEP PARA_328 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       STOP RUN.

      *> Sample COBOL source for lexer benchmarking
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCHMARK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MAX-ITEMS PIC 9(4) VALUE 332.
       01 WS-TOTAL PIC S9(9) VALUE ZERO.
       01 WS-NAME PIC X(16) VALUE 'bench-'.
       01 WS-I PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAX-ITEMS
               COMPUTE WS-TOTAL = WS-TOTAL + WS-I * 3
           END-PERFORM

       PARA_9109.
           DISPLAY 'STEP PARA_9109 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_6681.
           DISPLAY 'STEP PARA_6681 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_8152.
           DISPLAY 'STEP PARA_8152 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_5982.
           DISPLAY 'STEP PARA_5982 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_7950.
           DISPLAY 'STEP PARA_7950 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_2112.
           DISPLAY 'STEP PARA_2112 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       STOP RUN.

      *> Sample COBOL source for lexer benchmarking
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCHMARK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MAX-ITEMS PIC 9(4) VALUE 932.
       01 WS-TOTAL PIC S9(9) VALUE ZERO.
       01 WS-NAME PIC X(16) VALUE 'bench-'.
       01 WS-I PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAX-ITEMS
               COMPUTE WS-TOTAL = WS-TOTAL + WS-I * 5
           END-PERFORM

       PARA_5756.
           DISPLAY 'STEP PARA_5756 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_6601.
           DISPLAY 'STEP PARA_6601 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_1544.
           DISPLAY 'STEP PARA_1544 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_6115.
           DISPLAY 'STEP PARA_6115 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_4041.
           DISPLAY 'STEP PARA_4041 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_9563.
           DISPLAY 'STEP PARA_9563 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       STOP RUN.

      *> Sample COBOL source for lexer benchmarking
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENCHMARK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MAX-ITEMS PIC 9(4) VALUE 171.
       01 WS-TOTAL PIC S9(9) VALUE ZERO.
       01 WS-NAME PIC X(16) VALUE 'bench-'.
       01 WS-I PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAX-ITEMS
               COMPUTE WS-TOTAL = WS-TOTAL + WS-I * 7
           END-PERFORM

       PARA_9215.
           DISPLAY 'STEP PARA_9215 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_733.
           DISPLAY 'STEP PARA_733 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_4997.
           DISPLAY 'STEP PARA_4997 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_1986.
           DISPLAY 'STEP PARA_1986 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_3029.
           DISPLAY 'STEP PARA_3029 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       PARA_5691.
           DISPLAY 'STEP PARA_5691 START'
           IF WS-TOTAL > 1000 THEN
               COMPUTE WS-TOTAL = WS-TOTAL * 2
           ELSE
               COMPUTE WS-TOTAL = WS-TOTAL + 1
           END-IF
           DISPLAY 'TOTAL: ' WS-TOTAL

       STOP RUN.

