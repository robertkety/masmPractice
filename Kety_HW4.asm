TITLE Mean Program                                              (Kety_HW4.asm)

; Author: Robert Kety
; Modified: 06/23/2014
; Description:  Calculates and outputs ascending prime numbers at a quantity
;               determined by user input.  Maximum number of primes permitted
;               is stored in constant MAX_PRIMES. 1 is not considered prime.

INCLUDE Irvine32.inc

;Constants
USERNAME_LEN    = 32        ;Length of username including null pointer
MAX_PRIMES	    = 9500      ;Inclusive upper limit of numbers accepted
COL_WIDTH       = 8         ;Width of one column in 80 character row
NUM_COLS        = 10        ;Number of columns in each line of prime output

.data
;Variables - In order by use
insertTab   BYTE        9h, 0
intro1      BYTE        9h, 9h, 9h, " Welcome to the Prime Program!", 0
intro2      BYTE        9h, 9h, "     Programmed by Robert Kety (Homework 4)", 0
intro3      BYTE        9h, 9h, " This program is designed to output a quantity", 0
intro4      BYTE        9h, 9h, "      of prime numbers between 1 and 9500.", 0
prompt1     BYTE        "Please enter your name: ", 0
userName    BYTE        33 DUP(0)           ;USERNAME_LEN + 1 for Null pointer
greet1      BYTE        "Nice to meet you, ", 0
promptNum   BYTE        "Number of primes? Please enter an integer [1 - 9500]: ", 0
invalidQty  BYTE        "That number is out of range.", 0
lineCount   DWORD       9
primeQty    DWORD       0
primeArray  DWORD       9500 DUP(0)
primeCount  DWORD       ?           ;Number of primes stored in primeArray
spaceChar   BYTE        020h, 0
farewell    BYTE        "Goodbye, ", 0
exclamation BYTE        "!", 0

.code
main PROC
    call    Clrscr

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; Introduce program and author, provide description of program
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
	call    Intro

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; Get user name and greet
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
	;Pass reference of userName variable as an argument and call GetName
    push    OFFSET userName
    call    GetName

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; Get valid quantity of primes from user between 1 and 200
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
	;Pass reference of line count and quantity variable as an argument and 
    ;call GetPrimeQty
    push    OFFSET lineCount
    push    OFFSET primeQty
    call    GetPrimeQty
	
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; Output prime numbers based on user-defined quantity of desired primes
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
	;Pass reference of line count and quantity variable as an arguments and 
    ;call ShowPrimes
    push    OFFSET lineCount
    push    primeQty
    call    ShowPrimes
    
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
	; Say farewell to userName
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
	push    OFFSET userName
    call    GoodBye

    exit
main ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
Intro PROC Uses edx
; Output Title and Author Introduction
;   Receives:   None
;   Returns:    None, output to screen only
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
    ;No local variables
    ENTER 0, 0
    mov     edx, OFFSET intro1
    call    WriteString   ;"Welcome to the Prime Program!"
    call    CrLf        ;Move to next line

    mov     edx, OFFSET intro2
    call    WriteString   ;"Programmed by Robert Kety (Homework 4)"
    call    CrLf        ;Move to next line
    
    mov     edx, OFFSET intro3
    call    WriteString   ;"This program is designed to output a quantity of prime numbers"
    call    CrLf        ;Move to next line
    
    mov     edx, OFFSET intro4
    call    WriteString   ;"between 1 and 200."
    call    CrLf        ;Move to next line

    LEAVE
    ret
Intro ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
GetName PROC Uses edx ecx
; Prompt for user name, receive user name, and greet user
;   Receives:   OFFSET to BYTE for storing string up to USERNAME_LEN
;   Returns:    User name via OFFSET to userName variable
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
    ;No local variables
    ENTER 0, 0

    call    CrLf       

    ;Prompt for name
    mov     edx, OFFSET prompt1     ;"Please enter your name: "
    call    WriteString
	
    ;Read name
    mov     edx, [ebp + 16]
    mov     ecx, USERNAME_LEN
    call    ReadString

    ;Echo name with greeting
    mov     edx, OFFSET greet1
    call    WriteString          ;"Nice to meet you, "
    mov     edx, [ebp + 16]
    call    WriteString
    call    CrLf        ;Move to next line
    
    call    CrLf       

    LEAVE
    ret
GetName ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
GetPrimeQty PROC USES eax edx esi
;   Prompts user for a quantity of prime numbers from 1 to 200 until user
;   enters valid response.
;   Receives:   DWORD [EBP + 20] variable OFFSET (receives quantity of primes)
;               DWORD [EBP + 24] variable OFFSET (recevies line count)
;   Returns:    Stores quantity of primes in received variable via OFFSET
;               Stores incremented line count, if any
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
    ;No local variables
    ENTER 0, 0

    mov     esi, [ebp + 20]      ;Store offset location in esi

    ASK_QTY:			
            ;Prompt user for valid number and explain termination procedure
            mov     edx, OFFSET promptNum
            call    WriteString      ;"Number of primes? Please enter an integer [1 - 9500]: "
            
            ;Read input and post-test for validity
            call    ReadInt
            
            cmp     eax, 0
            jle     INVALID_QTY         ;Quantity must be > 0
            cmp     eax, MAX_PRIMES
            jg      INVALID_QTY         ;Quantity cannot be > MAX_PRIMES
            jmp     VALID_QTY
    INVALID_QTY:	
            mov     edx, OFFSET invalidQty
            call    WriteString      ;"That number is out of range."
            call    CrLf        ;Move to next line
            
            ;Increment line count by two lines
            push    esi
            mov     esi, [ebp + 24]
            mov     eax, [esi]
            add     eax, 2
            mov     [esi], eax
            pop     esi

            jmp     ASK_QTY		
    VALID_QTY:
            ;Store valid quantity in variable at argument location
            mov     [esi], eax
                       
    LEAVE
    ret
GetPrimeQty ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
ShowPrimes PROC Uses eax ebx ecx edx esi
;   Compute the quantity of primes using preceding primes as a basis for
;   determining successive primes via NextPrime procedure, primeArray (global
;   variable), and primeCount (global variable)
;   Receives:   DWORD [EBP + 28] variable (receives quantity of primes)
;               DWORD [EBP + 32] variable OFFSET (recevies line count)
;   Returns:    Stores incremented line count, if any
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
    ;One local variable
    ENTER 4, 0

    ;Set initial testing value
    mov     DWORD PTR [ebp - 4], 1

    ;Set initial pointer to array[0]
    mov     esi, OFFSET primeArray
    
    ;Load loop driver with quantity of prime numbers
    mov     ecx, [ebp + 28]
    NEXT_PRIME:        
        ;[ebp - 4] is passed as [ebp + 28] in NextPrime
        call    NextPrime
        
        ;Populate primeArray with value from NextPrime
            push    esi

            ;Calculate array index for array
            mov     eax, [ebp + 28]
            sub     eax, ecx
            mov     ebx, 4
            mul     ebx

            ;Shift esi pointer to new index location and copy value
            add     esi, eax
            mov     eax, [ebp - 4]
            mov     [esi], eax
            inc     primeCount      ;Ensure accurate element count
            
            pop     esi
        
        loop    NEXT_PRIME

    ;Set registers and local variables for Output_Loop
    mov     esi, OFFSET primeArray
        
    push    eax
    mov     eax, [esi]
    mov     [ebp - 4], eax
    pop     eax

    mov     ecx, primeCount
    
    OUTPUT_LOOP:
        ;[ebp - 4] is passed as [ebp + 20] in OutputPrime
        call    OutputPrime

        ;Increment line count every tenth prime
        mov     eax, [ebp + 28]     ;Load prime quantity max
        sub     eax, ecx            ;Subtract current prime count
        inc     eax                 ;Compensate for the loop decrement yet to occur
        mov     ebx, NUM_COLS       ;Number of columns
        cdq
        div     ebx
        cmp     edx, 0              ;if prime count does not divide evenly by the
        jne     NO_LINE_INC         ;number of columns, don't increment counter
                                    ;else, increment line counter variable
            ;Increment line counter
            push    esi
            push    eax
        
            mov     esi, [ebp + 32]
            mov     eax, [esi]
            inc     eax
            mov     [esi], eax          
        
            pop     eax
            pop     esi
        
        NO_LINE_INC:
        push    esi
        push    eax
        
            ;Pause output on every 25th line
            mov     esi, [ebp + 32]
            mov     eax, [esi]
            cmp     eax, 24
            jne     NO_PAUSE
            
            call    WaitMsg
            call    CrLf
            mov     eax, 0
            mov     [esi], eax
        
        NO_PAUSE:
        pop     eax
        pop     esi

        ;Load next value in array for output
        add     esi, 4
        push    eax
        mov     eax, [esi]
        mov     [ebp - 4], eax
        pop     eax
    
    loop    OUTPUT_LOOP
    
    LEAVE
    ret
ShowPrimes ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
NextPrime PROC Uses eax ebx ecx edx esi
;   Calculates the next ascending prime number.  This implementation is in
;   lieu of the isPrime procedure recommended in the assignment guidelines. It
;   performs the same calculation.  Denominators are determined from preceding
;   prime numbers stored in global variable primeArray.  To further limit the
;   amount of unneccessary calculations, this procedure only tests denominators
;   less than n/2, where n is the number being tested.
;   Receives:   Value of prime number or an initial value of 1. Value must be 
;               greater than 1.  Variable must be stored as DWORD at [ebp + 28]
;   Returns:    The next prime number to [ebp + 28]
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
    ;No local variables
    ENTER 4, 0

    ;Cycles through ascending numbers until it finds a prime
    mov     eax, [ebp + 28]
    NEXT_NUM:
        ;Set test value (n)
        inc     eax
        push    eax

        ;Set n / 2 and store in ecx and [ebp - 4]
        push    eax
        mov     ebx, 2
        cdq
        div     ebx
        mov     [ebp - 4], eax
        mov     ecx, [ebp - 4]
        pop     eax

        ;Test denominator range (1 < denominator < (n / 2))
        cmp     ecx, 1
        jle     FOUND_PRIME     ;Prime found is two or three

        ;Test all prime denominators in range against numerator n
        ;Set first denominator to primeArray[0]
        mov     esi, OFFSET primeArray
        mov     ebx, [esi]
        mov     ecx, primeCount
        NEXT_DENOM:
            ;Reset eax for next iteration of denominator division
            pop     eax
            push    eax

            ;Divide by current denominator and test for whole division
            cdq
            div     ebx
            cmp     edx, 0
            je      NOT_PRIME       ;If remainder == 0, kill loop (not prime)

            add     esi, 4          ;else, test next denominator
            mov     ebx, [esi]
            
            cmp     ebx, [ebp - 4]  ;If next denominator is >= n/2, terminate loop
            jg      KILL_LOOP

            jmp     END_LOOP

            NOT_PRIME:
                mov     eax, 0      ;Set to null as an indicator for next number
            
            KILL_LOOP:
                mov     ecx, 1      ;Set loop to exit condition
            
            END_LOOP:
            loop    NEXT_DENOM
        
        ;Test eax for next number indicator (eax == 0)
        cmp     eax, 0
        jne     FOUND_PRIME     ;If indicator is not 0, the number is prime!
        
        pop     eax             ;Reset eax to current number
        jmp     NEXT_NUM        ;otherwise, iterate to the next number

    FOUND_PRIME:
    pop     eax                 ;Reset eax to current number
    mov     [ebp + 28], eax     ;Return prime to calling function

    LEAVE
    ret
NextPrime ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
OutputPrime PROC Uses eax ecx edx
;   Outputs the prime number argument and determines the appropriate places
;   to pause output (every 25th line).  The call for this procedure has been
;   placed within the ShowPrimes function to further modularize the procedure.
;   Receives:   A prime number for formatted output to screen ([EBP + 20])
;   Returns:    None, output only
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
    ;No local variables
    ENTER 0, 0

    mov     eax, [ebp + 20]
    call    WriteDec

    ;Count Digits (Default is one digit)
    mov     ecx, 1
    NEXT_TENS:
        mov     ebx, 10
        cdq
        div     ebx

        cmp     eax, 0
        je      EXIT_LOOP
        
        inc     ecx
        jmp     NEXT_TENS
        
        EXIT_LOOP:
            
    ;COL_WIDTH less number of digits == number of spaces required
    mov     ebx, ecx
    mov     ecx, COL_WIDTH
    sub     ecx, ebx

    ;Add remaining spaces to ensure column width
    ADD_SPACE:
        mov     edx, OFFSET spaceChar
        call    WriteString

        loop    ADD_SPACE    

    LEAVE
    ret
OutputPrime ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
GoodBye PROC Uses edx
; Outputs some white space for readability and a farewell message
;   Receives:   User name OFFSET ([ebp + 12])
;   Returns:    None, output to screen only
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;
    ;No local variables
    ENTER 0, 0

    ; Extra white space for readability
    call    CrLf              
    call    CrLf       
            
    ;"Goodbye, [userName]!"
    mov     edx, OFFSET farewell
    call    WriteString  ;"Goodbye, "
    mov     edx, [ebp + 12]
    call    WriteString     ;"[userName]"
    mov     edx, OFFSET exclamation
    call    WriteString     ;"!"
    call    CrLf        ;Move to next line
    
    ; Extra white space for readability
    call    CrLf                   
    call    CrLf       
    
    LEAVE
    ret
GoodBye ENDP

END main