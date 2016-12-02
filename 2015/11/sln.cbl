000001 IDENTIFICATION DIVISION.
000002 PROGRAM-ID. ADVENT-OF-CODE-11.
000003 AUTHOR. DFYZ.
000004
000005 DATA DIVISION.
000006 WORKING-STORAGE SECTION.
000007 01 Password               PIC A(8).
000008 01 PasswordValidity       PIC 999 VALUE ZERO.
000009     88 PasswordIsValid            VALUE 1.
000010     88 PasswordIsInvalid          VALUE 0.
000011
000012 01 StraightCount          PIC 999 VALUE ZERO.
000013 01 ForbiddenLetterCount   PIC 999 VALUE ZERO.
000014 01 PairCount              PIC 999 VALUE ZERO.
000015 01 DistinctPairCount      PIC 999 VALUE ZERO.
000016
000017 01 ZCount                 PIC 999 VALUE ZERO.
000018 01 IncPos                 PIC 999 VALUE ZERO.
000019 01 NextCharCode           PIC 999 VALUE ZERO.
000020
000021 01 Letters                PIC X(26) VALUE
000022                           "abcdefghijklmnopqrstuvwxyz".
000023 01 LetterPair             PIC X(2).
000024 01 Idx                    PIC 999 VALUE ZERO.
000025
000026 PROCEDURE DIVISION.
000027 SolveTheProblem.
000028     ACCEPT Password
000029     PERFORM IncrementThePassword UNTIL PasswordIsValid
000030     DISPLAY Password
000031     STOP RUN.
000032
000033 IncrementThePassword.
000034     MOVE ZERO TO ZCount
000035     INSPECT FUNCTION REVERSE(Password) TALLYING
000036             ZCount FOR LEADING "z"
000037
000038     COMPUTE IncPos = FUNCTION LENGTH(Password) - ZCount
000039     MOVE ALL "a" TO Password(IncPos + 1:)
000040     COMPUTE NextCharCode = FUNCTION ORD(Password(IncPos:1)) + 1
000041     MOVE FUNCTION CHAR(NextCharCode) TO Password(IncPos:1)
000042
000043     PERFORM CheckPasswordValidity.
000044
000045 CheckPasswordValidity.
000046     SET PasswordIsValid TO TRUE
000047     PERFORM CheckStraight
000048     PERFORM CheckForbiddenCharacters.
000049     PERFORM CheckPairs.
000050
000051 CheckStraight.
000052     MOVE ZERO TO StraightCount
000053     PERFORM VARYING Idx FROM 2 BY 1 UNTIL Idx EQUALS 26
000054         INSPECT Password TALLYING StraightCount
000055         FOR ALL Letters(Idx - 1:3)
000056     END-PERFORM
000057     IF StraightCount EQUALS ZERO
000058         SET PasswordIsInvalid TO TRUE
000059     END-IF.
000060
000061 CheckPairs.
000062     MOVE ZERO TO DistinctPairCount
000063     PERFORM VARYING Idx FROM 1 BY 1 UNTIL Idx GREATER THAN 26
000064         MOVE ZERO TO PairCount
000065         STRING Letters(Idx:1), Letters(Idx:1) INTO LetterPair
000066         INSPECT Password TALLYING PairCount FOR ALL LetterPair
000067         IF PairCount GREATER THAN ZERO
000068             ADD 1 TO DistinctPairCount
000069         END-IF
000070     END-PERFORM
000071     IF DistinctPairCount LESS THAN 2
000072         SET PasswordIsInvalid TO TRUE
000073     END-IF.
000074
000075 CheckForbiddenCharacters.
000076     MOVE ZERO TO ForbiddenLetterCount
000077     INSPECT Password TALLYING
000078             ForbiddenLetterCount FOR ALL "i" "o" "l"
000079     IF ForbiddenLetterCount GREATER THAN ZERO
000080         SET PasswordIsInvalid TO TRUE
000081     END-IF.
