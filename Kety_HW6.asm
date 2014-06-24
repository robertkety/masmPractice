TITLE Homework 6     (Kety_HW6.asm)

; Author:   Robert Kety
; Modified: 06/23/2014
; Description:  This program prompts the user for 10 unsigned integers within
;               the range of DWORD.  The sum and mean are calculated and the
;               results are output to screen.

INCLUDE Irvine32.inc

NEGATIVE_SIGN = 02dh
MIN_NUM = 030h          ;The hexadecimal offset for the first digit '0' on the ASCII table
MAX_NUM = 039h
NUM_INTS = 10           ;The number of integers to receive from the user
FLOAT_LEN = 1000

getString macro     stringOffset:REQ
;   DESCRIPTION:    Prompts the user for an unsigned integer and loads input
;                   to string offset parameter.
;   RECEIVES:       stringOffset - The offset of a string with enough room 
;                                  for user input.
;   RETURNS:        stringOffset - Contains the input from the user.
    LOCAL   string
    .data
    string  BYTE    "Please enter an unsigned integer: ", 0

    .code
    push    ecx
    push    edx

    ;Prompt user for unsigned integer
    mov     edx, OFFSET string
    call    WriteString

    ;Store input to stringOffset
    lea     edx, stringOffset
    mov     ecx, (SIZEOF stringOffset) - 1
    call    ReadString
    
    pop     edx
    pop     ecx
ENDM

displayString macro stringOffset:REQ
;   DESCRIPTION:    Outputs string stored at parameter to user
;   RECEIVES:       stringOffset - The offset of a string
;   RETURNS:        Output to user
    push    edx

    mov     edx, stringOffset
    call    WriteString

    pop     edx
ENDM

.data
titleAuthor BYTE    "Sum and Mean", 9h, 9h, 9h, 9h, "Programmed by Robert Kety", 0dh, 0ah, 0dh, 0ah, 0
intro       BYTE    "This program accepts 10 unsigned DWORD integers and displays", 0dh, 0ah
            BYTE    "the input numbers, sum of numbers, and the mean.", 0dh, 0ah, 0
badString   BYTE    "Invalid unsigned integer. Please enter integer in range [0 ... 4294967295]", 0dh, 0ah, 0
userNumbers DWORD   NUM_INTS DUP (?)
arraySum    DWORD   ?
arrayMean   DWORD   ?
plusSign    BYTE    " + ", 0
equalSign   BYTE    " = ", 0
meanString  BYTE    "Mean: ", 0

.code
main PROC
    ;Introduce title, author, and program
    displayString OFFSET titleAuthor
    displayString OFFSET intro
    
    ;Populate array with NUM_INTS numbers from user
    lea     esi, userNumbers
    mov     ecx, NUM_INTS
    NEXT_INT:
        push    esi
        call    ReadVal
        add     esi, SIZEOF DWORD
        loop    NEXT_INT
    
    ;Calculate sum of numbers in array
    push    OFFSET arraySum
    push    OFFSET userNumbers
    push    NUM_INTS
    call    GetArraySum

    ;Calculate average of numbers in array
    push    OFFSET arrayMean
    push    arraySum
    push    NUM_INTS
    call    GetMean

    ;Display results to user
    push    arrayMean
    push    OFFSET meanString
    push    arraySum
    push    OFFSET equalSign
    push    OFFSET plusSign
    push    OFFSET userNumbers
    push    NUM_INTS
    call    DisplayResults

    exit
main ENDP

ReadVal PROC    USES    eax ebx ecx edx esi edi,
   pUserNum:PTR DWORD
;   DESCRIPTION:    Reads a string from user, error-checks to ensure string
;                   is an unsigned integer in the range of DWORD, and converts
;                   string to DWORD variable.  Requires getString macro.
;   RECEIVES:       pUserNum - A pointer to the location the user wants to store
;                              the converted number (Must be DWORD)
;   RETURNS:        pUserNum - Store the string input from user as a DWORD

    LOCAL   userString[256]:BYTE
    LOCAL   strLen:DWORD

    ;Initialize pUserNum to zero
    mov     edi, pUserNum
    mov     eax, 0
    mov     [edi], eax

    NEXT_STRING:
    cdq             ;NEXT_STRING loops based on flag stored in EDX, this resets that flag upon loop
    
    ;Prompt and receive user input
    getString   userString
    mov     strLen, eax

    std         ;I decided to read the string backwards in case I wanted to implement signed numbers later
    
    lea     esi, userString
    add     esi, eax        ;Move to end of char array since we're using std
    dec     esi             ;Compensate for null character
    
    mov     ecx, eax
    NEXT_CHAR:
        ;Store loop driver and decrement for proper index value
        push    ecx
        dec     ecx
        
        mov     eax, 0      ;Clear eax for next char
        
        lodsb               ;Load character to eax and move down array one index
        
        ;Confirm character value is within ASCII value ranges for 0 through 9
        cmp     eax, MIN_NUM
        jl      TERMINATE_LOOP
        cmp     eax, MAX_NUM
        jg      TERMINATE_LOOP
        
        ;Change character to integer digit
        sub     eax, MIN_NUM     ;The hexadecimal offset for the first digit '0' on the ASCII table

        ;Calculate power of 10 to apply to current number (based on its index value)
        mov     ebx, strLen
        dec     ebx         ;Compensate for null terminator
        sub     ebx, ecx    ;Determine power of ten for that index (i.e., 10^ebx)
        
        cmp     ebx, 0
        je      NO_POWER    ;10^0 needs to skip the POWER_TEN subroutine
                
        ;Multiply number by power of 10 equivalent to its place in the final number
        push    ecx
        mov     ecx, ebx
        POWER_TEN:
            mov     ebx, 10
            mul     ebx
            loop    POWER_TEN
        pop     ecx
        
        NO_POWER:
        ;Retrieve current subtotal
        mov     ebx, [edi]
        add     ebx, eax
        
        ;Terminate if > 0xFFFFFFFFh
        jc      TERMINATE_LOOP

        ;Store new subtotal
        mov     [edi], ebx
        jmp     CONTINUE_LOOP

        TERMINATE_LOOP:
        mov     edx, OFFSET badString
        call    WriteString
        
        ;Reset pUserNum to avoid partial returns
        mov     eax, 0
        mov     [edi], eax
        mov     edx, 0FFFFFFFFh          ;This is a flag to signal another iteration of NEXT_STRING

        ;Set loop driver to exit loop condition
        pop     ecx
        mov     ecx, 1
        push    ecx
        
        CONTINUE_LOOP:
        pop     ecx     ;Restores loop driver
        loop    NEXT_CHAR
    
    cmp     edx, 0FFFFFFFFh               ;This flag to signals another iteration of NEXT_STRING
    je      NEXT_STRING

    ret 4
ReadVal ENDP

WriteVal PROC   USES eax ebx edx edi,
    pValue:DWORD
;   DESCRIPTION:    Recursively outputs DWORD value to screen by converting 
;                   it to a string and using the displayString macro to output.
;                   The design of this procedure was influenced by the Irvine
;                   procedure WriteDec.
;   RECEIVES:       pValue - The value to be output to user.  
;   RETURNS:        Output to user
    LOCAL   stringChar[2]:BYTE      ;Holds the character currently being output

    mov     eax, pValue

    ;Isolate the right-most digit in the number into edx
    mov     ebx, 10
    mov     edx, 0
    div     ebx
       
    cmp     eax, 0  ;No more right-most digits when eax is zero
    je      BASE_CASE       ;Base Case
        
    push    eax
    call    WriteVal        ;Recursive Case
       
    BASE_CASE:
    lea     edi, stringChar
    
    xchg    eax, edx        ;Place right-most digit in eax
        
    add     eax, MIN_NUM    ;Convert number to char
    stosb                   ;Record char to string
        
    mov     eax, 0
    stosb                   ;Recod NULL terminator to string

    lea     edx, stringChar ;Reset string pointer
        
    displayString edx

    ret 4
WriteVal ENDP

GetArraySum PROC    USES eax ecx esi,
    pNumInts:DWORD,
    pNumArray:PTR DWORD,
    pArraySum:PTR DWORD
;   DESCRIPTION:    Computes the sum of values stored in numArray.  Based on a 
;                   function I wrote for Kety_HW3.asm.
;   Receives:   pNumArray - OFFSET to filled DWORD array with pNumInts elements 
;               pNumInts - A counter variable already set to quantity of 
;                          pNumArray elements
;               pArraySum - OFFSET for return variable 
;   Returns:    pArraySum, the sum of values stored in numArray

    mov     esi, pNumArray
    mov     ecx, pNumInts

    ;Initialize EAX to zero
    mov     eax, 0				;Set eax to 0

    ;Sum all values stored in array
    SUM_LOOP:
            add     eax, [esi]
            add     esi, TYPE DWORD
            loop SUM_LOOP
    ;Store sum in return variable
    mov     esi, pArraySum
    mov     [esi], eax

    ret	12
GetArraySum ENDP

GetMean PROC    USES eax ebx edx edi,
    pIntNum:DWORD,
    pSumArray:DWORD,
    pMeanArray:PTR DWORD
;   DESCRIPTION:    Computes the mean of a sum. Based on a function I wrote 
;                   for Kety_HW3.asm.
;   Receives:   pIntNum - The number of elements to divide sum (denominator)
;               pSumArray - The sum to divide (numerator)
;               pMeanArray - OFFSET to return variable
;   Returns:    pMeanArray - The mean of pArraySum and pNumInts
    mov     eax, pSumArray
    mov     ebx, pIntNum
    cdq
    div     ebx

    mov     edi, pMeanArray
    mov     [edi], eax

    ret 12
GetMean ENDP

DisplayResults  PROC USES ecx esi,
    pNumInts:DWORD,
    pNumArray:PTR DWORD,
    pPlusSign:PTR BYTE,
    pEqualSign:PTR BYTE,
    pArraySum:DWORD,
    pMeanString:PTR BYTE,
    pArrayMean:DWORD
;   DESCRIPTION:    Outputs results of Homework 6 assignment to user.
;                   Specifically, it repeats the numbers entered, their sum,
;                   and the mean of the values.
;   RECIEVES:       pArrayMean - The mean value of the array of numbers
;                   pMeanString - A string introducing pArrayMean
;                   pArraySum - The sum of the array of numbers
;                   pEqualSign - A string containing " = "
;                   pPlusSign - A string containing " + "
;                   pNumArray - The array of numbers input from user
;                   pNumInts - The quantity of elements in pNumArray
;   RETURNS:        Output to user
    
    call    CrLf    ;Produce some white space for output

    ;Output values in pNumArray and a " + ", except for the last
    ;value which will have a " = ". (i.e., "1 + 2 = "
    mov     esi, pNumArray
    mov     ecx, pNumInts
    NEXT_NUMBER:
        push    [esi]
        call    WriteVal

        cmp             ecx, 1
        je              EQUAL_SIGN
        
        displayString   pPlusSign
        jmp             END_LOOP

        EQUAL_SIGN:
        displayString   pEqualSign
        
        END_LOOP:
        add             esi, SIZEOF DWORD
        loop            NEXT_NUMBER

    ;Output array sum
    push    pArraySum
    call    WriteVal
    call    CrLf

    ;Output array mean
    displayString   pMeanString
    push    pArrayMean
    call    WriteVal
    
    ;Some final white space to close out the output
    call    CrLf
    call    CrLf

    ret 28
DisplayResults  ENDP

END main