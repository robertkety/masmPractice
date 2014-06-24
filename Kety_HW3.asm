TITLE Mean Program                                              (Kety_HW3.asm)

; Author: Robert Kety
; Modified: 06/23/2014
; Description:  Calculates the mean of a series of numbers from 0 to 100,
;               inclusive. User input of numbers terminates upon first use
;               of negative number.  Maximum number entries from user is
;               stored in constand MAX_ARRAY

INCLUDE Irvine32.inc

;Constants
USERNAME_LEN    = 32        ;Length of username including null pointer
MAX_ARRAY       = 500       ;The maximum number of user numbers available to 
                            ;user including null pointer
UPPER_LIMIT	    = 100       ;Inclusive upper limit of numbers accepted
FLOAT_LEN       = 1000      ;Round to -th place

.data
;Variables - In order by use
insertTab   BYTE        9h, 0
intro1      BYTE        9h, "Welcome to the Mean Program!", 0   ;Centered Header
intro2      BYTE        "Programmed by Robert Kety (Homework 3)", 0
intro3      BYTE        "This program is designed to compute the average", 0
intro4      BYTE        "of a series of numbers from 0 to 100.", 0
prompt1     BYTE        "Please enter your name: ", 0
userName    BYTE        33 DUP(0)           ;USERNAME_LEN + 1 for Null pointer
greet1      BYTE        "Nice to meet you, ", 0
promptNum   BYTE        "Please enter an integer from [0 - 100].", 0
stopInstr   BYTE        "Enter a negative number when you are finished: ", 0
invalidQty  BYTE        "That number is out of range.", 0
noInput     BYTE        "No numbers entered - calculation terminated.", 0
lineCount   DWORD       0
numArray    DWORD       MAX_ARRAY DUP(?)
numCount    DWORD       0
sum         DWORD       0
meanWhole   DWORD       0
meanFloat   DWORD       0
meanInt     DWORD       0
zeroQty     DWORD       0       ;Number of leading zeros in meanFloat
results1    BYTE        "The sum of the ", 0
results2    BYTE        " numbers entered was ", 0
results3    BYTE        ", ", 0
results4    BYTE        "which has a rounded average of ", 0
results5    BYTE        " (integer) or ", 0
results6    BYTE        " (floating point).", 0
decPoint    BYTE        ".", 0
farewell    BYTE        "Goodbye, ", 0
exclamation BYTE        "!", 0

.code
main PROC
    call        Clrscr

    ;Initialize ECX with lineCount
    mov     ecx, lineCount

    mov     eax, 8
    mov     ebx, 0BF5h
    mul     ebx

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; Output Title and Author Introduction
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    mov     edx, OFFSET intro1
    call    NumWriteString   ;"     Welcome to the Mean Program!"
    call    CrLf        ;Move to next line

    mov     edx, OFFSET intro2
    call    NumWriteString   ;"Programmed by Robert Kety (Homework 3)"
    call    CrLf        ;Move to next line
    
    mov     edx, OFFSET intro3
    call    NumWriteString   ;"This program is designed to compute the average"
    call    CrLf        ;Move to next line
    
    mov     edx, OFFSET intro4
    call    NumWriteString   ;"of a series of numbers from 0 to 100."
    call    CrLf        ;Move to next line

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; Prompt for user name, receive user name, and greet user
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    call    NumCrLf     ;Insert numbered blank line

    ;Prompt for name
    mov     edx, OFFSET prompt1     ;"Please enter your name: "
    call    NumWriteString
	
    ;Read name
    mov     edx, OFFSET userName
    mov     ecx, USERNAME_LEN
    call    ReadString

    ;Echo name with greeting
    mov     edx, OFFSET greet1
    call    NumWriteString          ;"Nice to meet you, "
    mov     edx, OFFSET userName
    call    WriteString
    call    CrLf        ;Move to next line
    
    call    NumCrLf     ;Insert numbered blank line

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; Get valid numbers from user between 0 to 100
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
	
    ;Initialize the count at 0
    mov     numCount, 0

    ;Pass references to array and count arguments and call GetNum
    push    OFFSET numArray
    push    OFFSET numCount
    call    GetNum
	
    cmp     numCount, 0     ;If no numbers were input, jump to goodbye message
    je      GOODBYE

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; Compute sum of numbers in numArray
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
	
    ;Pass array reference and count value arguments and call GetArraySum 
    push    OFFSET numArray
    push    numCount
    call    GetArraySum

    mov     sum, eax        ;Store sum of numbers
	
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; Compute rounded mean
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
	
    ;Pass sum and count value arguments and call GetMean
    push    sum
    push    numCount
    call    GetMean

    mov     meanWhole, eax      ;Store whole numbers for mean
    mov     meanFloat, ebx      ;Store floating point numbers for mean
    mov     zeroQty, ecx        ;Store leading zeros for floating point
    mov     meanInt, edx        ;Store integer numbers for mean
	
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; Output results to user
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
	
    call    NumCrLf     ;Insert numbered blank line
    
    ;"The sum of the X numbers entered was Y,"
    mov     edx, OFFSET results1    
    call    NumWriteString          ;"The sum of the "
    mov     eax, numCount           
    call    WriteDec                ;"X"
    mov     edx, OFFSET results2
    call    WriteString             ;" numbers entered was "
    mov     eax, sum
    call    WriteDec                ;"Y"
    mov     edx, OFFSET results3
    call    WriteString             ;","
    call    CrLf        ;Move to next line

    ;"which has a rounded average of B (integer) or Z.### (floating point)."
    mov     edx, OFFSET results4
    call    NumWriteString          ;"which has a rounded average of "
    mov     eax, meanInt
    call    WriteDec                ;"B"
    mov     edx, OFFSET results5    
    call    WriteString             ;" (integer) or "
    mov     eax, meanWhole
    call    WriteDec                ;"Z"
    mov     edx, OFFSET decPoint
    call    WriteString             ;"."
    
        ;Output any leading zeros in the tenths and hundredths place
        mov     eax, 0
        mov     ecx, zeroQty    ;Number of leading zeros
        ZERO_POINT:
            cmp     ecx, 0
            je      EXIT_LOOP       ;If no zeros, exit loop
            call    WriteDec        ;"0"
            jmp     END_OF_LOOP
            EXIT_LOOP:
                    mov     ecx, 1  ;Set exit loop conditions
            END_OF_LOOP:
            loop    ZERO_POINT
    
    mov     eax, meanFloat
    call    WriteDec            ;"###"

    mov     edx, OFFSET results6
    call    WriteString             ;" (floating point)."
    
    call    CrLf        ;Move to next line

    ; Extra white space for readability
    call    NumCrLf     ;Insert numbered blank line
    call    NumCrLf     ;Insert numbered blank line
    
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; Goodbye!
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    GOODBYE:
            ;"Goodbye, [userName]!"
            mov     edx, OFFSET farewell
            call    NumWriteString  ;"Goodbye, "
            mov     edx, OFFSET userName
            call    WriteString     ;"[userName]"
            mov     edx, OFFSET exclamation
            call    WriteString     ;"!"
            call    CrLf        ;Move to next line
    
            ; Extra white space for readability
            call    NumCrLf     ;Insert numbered blank line
            call    NumCrLf     ;Insert numbered blank line
    
    exit
main ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
NumWriteString  PROC
;   Outputs current line count, the string in edx, and increments lineCount
;   Receives:   EDX, OFFSET of string to output
;   Returns:    linecount is incremented
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
    ;Store EAX and EDX registers
    push    edx
    push    eax

    ;Output current lineCount in decimal and insert tab
    mov     eax, lineCount
    call    WriteDec
    mov     edx, OFFSET insertTab
    call    WriteString

    ;Restore EAX and EDX registers
    pop     eax
    pop     edx

    ;Write argument string
    call    WriteString

    ;Increment use of lineCount
    inc     lineCount

    ret
NumWriteString  ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
NumCrLf PROC
;   Outputs current line count, moves to next line, and increments lineCount
;   Receives:   None.
;   Returns:    lineCount is incremented
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
    ;Store EAX register
    push    eax

    ;Output current lineCount in decimal and move to next line
    mov     eax, lineCount
    call    WriteDec
    call    CrLf        ;Move to next line

    ;Restore EAX register
    pop     eax

    ;Increment use of lineCount
    inc     lineCount

    ret
NumCrLf ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
GetNum PROC	USES edx
;   Prompts user for a number from 0 to 100 until the user inputs a negative
;   number.
;   Receives:   OFFSET to empty DWORD array with capacity of MAX_ARRAY and 
;               the number of elements in the array.
;   Returns:    None, values are stored in memory locations for numArray and
;               numCount
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
    ;No local variables
    ENTER   0, 0
    
    ;Store ESI and EDI registers
    push    esi
    push    edi

    ;Move references of variables to registers
    mov     esi, [ebp + 16]		;Move array OFFSET to esi
    mov     edi, [ebp + 12]		;Move count OFFSET to ecx
    
    ASK_QTY:			
            ;Prompt user for valid number and explain termination procedure
            mov     edx, OFFSET promptNum
            call    NumWriteString      ;"Please enter an integer from [0 - 100]."
            call    CrLf        ;Move to next line
            
            mov     edx, OFFSET stopInstr
            call    NumWriteString      ;"Enter a negative number when you are finished: "
			
            ;Read input and post-test for validity
            call    ReadInt
            cmp     eax, 0
            jl      EXIT_PROC           ;Exit when user enters number < 0
            cmp     eax, UPPER_LIMIT
            jg      INVALID_QTY         ;Cannot be > 100
            jmp     VALID_QTY
    INVALID_QTY:	
            mov     edx, OFFSET invalidQty
            call    NumWriteString      ;"That number is out of range."
            call    CrLf        ;Move to next line
            call    NumCrLf     ;Insert numbered blank line
            jmp     ASK_QTY		
    VALID_QTY:
            ;Store valid quantity in array, point to next memory location, loop
            mov     [esi], eax
            add     esi, TYPE DWORD
            inc     DWORD PTR [edi] 
            jmp     ASK_QTY
    NO_INPUT:
            mov     edx, OFFSET noInput
            call    NumWriteString      ;"No numbers entered - calculation terminated."
            call    CrLf        ;Move to next line
            call    NumCrLf     ;Insert numbered blank line
            jmp     END_PROC
    EXIT_PROC:
            cmp     DWORD PTR [edi], 0      ;If no numbers were output, inform user 
            je      NO_INPUT
    END_PROC:
            call    NumCrLf     ;Insert numbered blank line					;extra space for readability
            
            ;Restore EDI and ESI registers
            pop     edi
            pop     esi
            
            LEAVE
            ret			
GetNum ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
GetArraySum PROC
;   Computes the sum of values stored in numArray
;   Receives:   OFFSET to empty DWORD array with capacity of MAX_ARRAY and 
;               OFFSET to a counter variable already set to 0 
;   Returns:    EAX, the sum of values stored in numArray
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
    ;No local variables
    ENTER   0, 0
    
    ;Store ESI and ECX registers
    push    esi
    push    ecx

    ;Move references of variables to registers
    mov     esi, [ebp + 12]		;Move array OFFSET to esi
    mov     ecx, [ebp + 8]		;Move count to ecx

    ;Initialize EAX to zero
    mov     eax, 0				;Set eax to 0

    ;Sum all values stored in array
    SUM_LOOP:
            add     eax, [esi]
            add     esi, TYPE DWORD
            loop SUM_LOOP

    ;Restore ECX and ESI registers
    pop     ecx
    pop     esi
    
    LEAVE
    ret	
GetArraySum ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
GetMean PROC
;   Computes the mean of a sum and the number of elements in an array.  Rounds
;   to the whole number.
;   Receives:   The sum of a set of numbers and the quantity of those numbers
;   Returns:    EAX, the whole numbers for mean
;               EBX, the floating point numbers for mean (i.e., decimal number)
;               ECX, leading zeros in floaing point number (e.g., 2 for .001)
;               EDX, the integer numbers for mean
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
    ;Zero local variables
    ENTER   0, 0

    ;Move references of variables to registers and divide
    mov     eax, [ebp + 12]
    mov     edx, FLOAT_LEN  ;Numerator * FLOAT_LEN to determine floating point values
    mul     edx

    mov     ebx, [ebp + 8]
    
    cdq         ;EDX:EAX
    div     ebx

    ;Round quotient (eax) based on remainder (edx) * 2 and numerator comparison
    push    eax         ;Store Quotient
    mov     eax, 2
    mul     edx         ;Remainder * 2 to EAX
    mov     edx, eax    ;Move Remainder * 2 to EDX
    pop     eax         ;Restore Quotient
    cmp     ebx, edx    ;If Numerator > Remainder * 2, then round down
    jg      ROUND_DOWN
    inc     eax         ;else, round up
    ROUND_DOWN:
            ;Do nothing to round down
    
    ;Calculate whole number and floating-point numbers of mean
        push    eax             ;Store rounded quotient

        ;Determine whole number of mean as Quotient / FLOAT_LEN
        mov     ebx, FLOAT_LEN
        cdq
        div     ebx

        push    eax             ;Store whole number of mean

        ;Determine floating-point number as:
        ;Quotient - (whole number * FLOAT_LEN)
        mul     ebx             ;Whole number * FLOAT_LEN
        mov     ebx, eax        ;Whole number * FLOAT_LEN to ebx
        pop     eax             ;Whole number
        mov     ecx, eax        ;Whole number to ecx
        pop     eax             ;Quotient
        sub     eax, ebx        ;Quotient - (whole number * FLOAT_LEN)
        mov     ebx, eax        ;Floating point number to ebx
        mov     eax, ecx        ;Whole number to eax

        ;Calculate leading zeros, if any in floating point number
        ;(e.g., .011, .001, .000).  Max lead zeros is number of zeros in 
        ;FLOAT_LEN minus 1 (two, using the previous example).
        
            ;Set ecx to quantity of zero number of leading zeros
            mov     ecx, 0

            cmp     ebx, 100        ;Tenths comarison
            jge     NO_LEAD_ZEROS   ;If no tenths, then no lead zeros
            inc     ecx             ;else, increment
            
            cmp     ebx, 10         ;Hundredths comparison
            jge     NO_LEAD_ZEROS   ;If no thousandths, then no lead zeros
            inc     ecx             ;else, increment
    
            ;There is no Thousandths comparison as this would be == 0

        NO_LEAD_ZEROS:
            ;ecx remains at 0

    ;Calculate and round integer mean and store in EDX
        ;Store whole number mean
        push    eax
        push    ebx

        mov     edx, eax        
        push    edx             ;Store whole number as integer number

        ;Round whole number (eax) based on floating number (ebx), increment edx
        ;Determine round up if the following formula is not zero:
        ;(floating number * 2) / FLOAT_LEN
                ;(floating number * 2)
                mov     eax, ebx
                mov     ebx, 2
                mul     ebx

                ;(floating number * 2) / FLOAT_LEN
                mov     ebx, FLOAT_LEN
                cdq
                div     ebx

        pop     edx         ;Restore whole number for possible increment
        
        cmp     eax, 0
        je     ROUND_DOWN2  ;if result is zero, round down
        inc     edx         ;else, round up
        ROUND_DOWN2:
            ;Do nothing to round down

        ;Restore whole number mean
        pop     ebx
        pop     eax
            
    LEAVE
    ret	
GetMean ENDP

END main