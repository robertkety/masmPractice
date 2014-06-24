TITLE Sorting Randoms     (Kety_HW5.asm)

; Author:   Robert Kety
; Modified: 06/23/2014
; Description:  Generates an array of random numbers (quantity determined by
;               user input), sorts the array, and calculates the median.
;               Results are displayed to the user as the 1) unsorted list,
;               2) the median, and 3) the sorted list.
;               This program includes extra credit option of sorting the
;               array using a recursive heap sort algorithm, outputting
;               the array by column instead of row, outputting and inputting
;               values to/from a file (i.e. "array.txt"), and displays the
;               median in scientific notation using the FPU.  I chose to 
;               process the median throgh FPU alone since it would be a waste
;               of processing time to cast each array value from REAL10 to 
;               DWORD for output.  Instead, I've store the array in DWORD and
;               cast the values relevant to median calculation to REAL10.
;               This method has the added benefit of not violating the 10 column 
;               formatting standard with scientific notation from repeated
;               calls to WriteFloat.
; NOTE!:        The OpenFile procedure is supposed to be part of the Irvine32  
;               library, but Visual Studio could not locate the procedure. The
;               procedure has been reproduced here from the Assembly Language 
;               for x86 Processors text book.  You may need to remove the 
;               implementation for this procedure if your version of the 
;               Irvine32 library includes OpenFile.

INCLUDE Irvine32.inc

MIN =   10
MAX =   200
LO  =   100
HI  =   1000
DWORD_BYTES = 4
NUM_COLUMNS = 10

.data
titleAuthor BYTE    "Sorting Randoms", 9h, 9h, 9h, 9h, "Programmed by Robert Kety", 0dh, 0ah, 0dh, 0ah, 0
intro       BYTE    "This program generates random numbers in the range [100 .. 999],", 0dh, 0ah
            BYTE    "displays the original list, sorts the list, and calculates the", 0dh, 0ah
            BYTE    "median value. Finally, it displays the list sorted in descending", 0dh, 0ah
            BYTE    "order.", 0dh, 0ah, 0dh, 0ah, 0
promptRnge  BYTE    "How many numbers should be generated? [10 .. 200]: ", 0
invalidRnge BYTE    "Invalid input!", 7h, 0dh, 0ah, 0
fileName    BYTE    "array.txt", 0
fileHandle  HANDLE  ?
badFile     BYTE    "Cannot create file! Writing to memory instead of file.", 0dh, 0ah, 0
qtyElements DWORD   ?
array       DWORD   MAX DUP (0)
unsortedMsg BYTE    "The unsorted random numbers:", 0dh, 0ah, 0
medianMsg   BYTE    "The integer median is ", 0
fMedianMsg  BYTE    "The floating point median is ", 0
periodCrLf  BYTE    ".", 0dh, 0ah, 0dh, 0ah, 0
sortedMsg   BYTE    "The sorted list:", 0dh, 0ah, 0

.code
main PROC
    call    Randomize   ;Seed the random number generator

    ;Output program title and author
    push    OFFSET titleAuthor
    push    OFFSET intro
    call    introProgram
    
    ;Prompt and retrieve quanty of elements from user
    push    OFFSET promptRnge
    push    OFFSET invalidRnge
    push    OFFSET qtyElements
    call    requestRange                ;get data procedure
    
    ;Create a file to receive qtyElements number of randomly generated values
    mov     edx, OFFSET fileName
    call    CreateOutputFile
    cmp     eax, INVALID_HANDLE_VALUE
    je      WRITE_TO_MEMORY             ;If the filename is bad, use memory instead of hard drive
    mov     fileHandle, eax

    ;Output qtyElements number of randomly generated values to file
    push    qtyElements
    push    fileHandle
    call    foutArray

    ;Close output file
    mov     eax, fileHandle
    call    CloseFile

    ;Open file for input
    mov     edx, OFFSET fileName
    call    OpenFile
    cmp     eax, INVALID_HANDLE_VALUE
    je      WRITE_TO_MEMORY             ;If the filename is bad, use memory instead of hard drive
    mov     fileHandle, eax

    ;Retrieve array of values from input file
    push    SIZEOF array
    push    OFFSET array
    push    fileHandle
    call    finArray
    
    ;Close input file
    mov     eax, fileHandle
    call    CloseFile
   
    jmp     ARRAY_POPULATED             ;Do not repopulate array with memory method
   
    WRITE_TO_MEMORY:                    ;This method used if file operation fails
        ;Inform user that memory method is being used due to a bad file name
        mov     edx, OFFSET badFile         
        call    WriteString

        push    qtyElements
        push    OFFSET array
        call    populateArry    ;fill array
    
    ARRAY_POPULATED:
    push    OFFSET unsortedMsg
    push    qtyElements
    push    OFFSET array
    call    displayList     ;display sorted list
    
    push    qtyElements
    push    OFFSET array
    call    sortArray       ;sort list
    
    push    OFFSET periodCrLf
    push    OFFSET fMedianMsg
    push    OFFSET medianMsg
    push    qtyElements
    push    OFFSET array
    call    dispMedian      ;display median
    
    push    OFFSET sortedMsg
    push    qtyElements
    push    OFFSET array
    call    displayList     ;display sorted list
    
    exit	                ; exit to operating system
main ENDP

introProgram    PROC    USES edx,
    pIntro:PTR BYTE,
    pTitleAuth:PTR BYTE
;   DESCRIPTION:    Introductory output for user.
;   RECEIVES:       pTitleAuth - Title and author information (string)
;                   pIntro     - Description of program (string)
;   RETURNS:        NONE.

    mov     edx, pTitleAuth
    call    WriteString

    mov     edx, pIntro
    call    WriteString

    ret 8
introProgram    ENDP

requestRange    PROC    USES eax edx,       ;get data procedure
    pQtyElements:PTR DWORD,
    pInvalidRnge:PTR BYTE,
    pPromptRnge:PTR BYTE
;   DESCRIPTION:    Prompts the user for the size of an array in the range set
;                   by global constants MIN and MAX.
;   RECEIVES:       pQtyElements - The return variable which receive the users
;                                  input.
;                   pInvalidRnge - A string telling the user entry was invalid.
;                   pPromptRnge  - A string asking the user to enter within a
;                                  range of acceptable values.
;   RETURNS:        pQtyElements - The quantity of elements selected by user

    ;Prompt continually until user enters number within range
    PROMPT_RANGE:
        ;Prompt for input
        mov     edx, pPromptRnge
        call    WriteString

        ;Receive input
        call    ReadInt

        ;Confirm input is within range values MIN and MAX
        cmp     eax, MIN
        jl      INVALID_RANGE
        cmp     eax, MAX
        jle     VALID_RANGE
        
        INVALID_RANGE:
        ;Output invalid and prompt for new input by jumping to top of loop
        mov     edx, pInvalidRnge
        call    WriteString
        jmp     PROMPT_RANGE

        VALID_RANGE:
        ;Store valid result in return variable
        mov     esi, pQtyElements
        mov     [esi], eax

    ret 12
requestRange    ENDP

foutArray       PROC    USES eax ecx edx,
    pFileHandle:DWORD,
    pQtyElements:DWORD
;   DESCRIPTION:    Generates random integers within the range of LO to HI and
;                   outputs the number to a file.
;   RECEIVES:       pFileHandle -   The handle for a file currenly open for 
;                                   output.
;                   pQtyElements -  The number user selected elements to 
;                                   generate.
;   RETURNS:        NONE! Data is output to file associated with pFileHandle
    LOCAL   currNum:DWORD

    mov     ecx, pQtyElements
    FOUT_NEXT:
        ;Call random range with the value of HI less LO in eax.  Then, add
        ;LO to random number returned in eax to put number in the range
        ;of LO to HI
        mov     eax, HI
        sub     eax, LO
        call    RandomRange
        add     eax, LO
        mov     currNum, eax

        push    ecx     ;Store loop counter during call to WriteToFile

        ;Write number to file
        mov     eax, pFileHandle
        lea     edx, currNum
        mov     ecx, DWORD_BYTES
        call    WriteToFile

        pop     ecx     ;Restore loop counter

        loop    FOUT_NEXT

    ret 8
foutArray       ENDP

finArray        PROC    USES eax ecx edx,
    pFileHandle:DWORD,
    pArray:PTR DWORD,
    pSizeOfArray:DWORD
;   DESCRIPTION:    Retrieves input buffer from file and copies it to the
;                   array parameter.
;   RECEIVES:       pFileHandle -   The handle for a file currently open for
;                                   input.
;                   pArray -    The array to receive input from file (MUST HAVE
;                               CAPACITY OF pSizeOfArray!)
;                   pSizeOfArray -  The byte size of the buffer to be read from
;                                   the file and copied to pArray.
;   RETURNS:        pArray -    Populated with the values stored in the file.
    ;Copy file data to array
    mov     eax, pFileHandle
    mov     edx, pArray
    mov     ecx, pSizeOfArray
    call    ReadFromFile
    
    ret 12
finArray        ENDP

;------------------------------------------------------------------------------
OpenFile        PROC
;
;   Opens a new text file and opens for input.
;   Receives:   EDX points to the filename.
;   Returns:    If the file was opened successfully, EAX
;   contains a valid file handle. Otherwise, EAX equals
;   INVALID_HANDLE_VALUE.
;
;   NOTE!:  This procedure is supposed to be part of the Irvine32 library, but 
;           Visual Studio could not locate the procedure. The procedure has
;           been reproduced here from the Assembly Language for x86 Processors 
;           text book.
;------------------------------------------------------------------------------
    INVOKE  CreateFile,
        edx, GENERIC_READ, DO_NOT_SHARE, NULL,
        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
    ret
OpenFile        ENDP

populateArry    PROC    USES eax ebx ecx esi,   ;fill array
    pArray:PTR DWORD,
    pQtyElements:DWORD
;   DESCRIPTION:    Populates an array with random values.  Requires call to 
;                   randomize before execution!  Range of random numbers 
;                   determined by global constants HI and LO
;   RECEIVES:       pArray - Pointer to an Array (contains at least one element)
;                   pQtyElements - The size of the array
;   RETURNS:        It populates the elements of pArray with random values

    ;Move to beginning of array and set loop driver for the end of the array
    mov     esi, pArray
    mov     ecx, pQtyElements
    ADD_NEXT:
        ;Call random range with the value of HI less LO in eax.  Then, add
        ;LO to random number returned in eax to put number in the range
        ;of LO to HI
        mov     eax, HI
        sub     eax, LO
        call    RandomRange
        add     eax, LO

        ;Store value in array and move to next position in the array
        mov     [esi], eax
        add     esi, DWORD_BYTES
        loop    ADD_NEXT
    
    ret 8
populateArry    ENDP

displayList     PROC    USES eax ecx edx esi,  ;display sorted list
    pArray:PTR DWORD,
    pQtyElements:DWORD,
    pSortedMsg:PTR BYTE
;   DESCRIPTION:    Displays the values stored in an array preceded by a
;                   string that should describe the values being output. Output
;                   is displayed by column then row instead of row then column.
;   RECEIVES:       pArray - Pointer to an Array (contains at least one element)
;                   pQtyElements - The size of the array
;                   pSortedMsg - Message describing values being output.
;   RETURNS:        NONE!

    LOCAL   tabChar[2]:BYTE,    ;Local string variable
            rowOffset:DWORD,    ;The number of bytes separating each element on next row
            extraRow:DWORD,     ;The number of elements remaining after deducting full rows
            outCount:DWORD      ;The number of elements output
    
    ;Equivalent to: tabChar    BYTE    9h, 0
    mov     tabChar[0], 9h      
    mov     tabChar[1], 0
    
    ;Store the number of bytes separating the first element of each row in a 10 column table
    mov     eax, pQtyElements
    mov     ebx, NUM_COLUMNS
    cdq
    div     ebx
    mov     extraRow, edx

    ;Compute and store the number of bytes between rows
    mov     ebx, DWORD_BYTES
    mul     ebx
    mov     rowOffset, eax

    ;Initialize outCount
    mov     outCount, 0

    ;Describe values being output
    mov     edx, pSortedMsg
    call    WriteString

    ;Compute the number of rows to output to drive outer output loop
    mov     eax, pQtyElements
    mov     ebx, NUM_COLUMNS
    cdq
    div     ebx
    cmp     edx, 0
    jne     UNEQUAL_ROWS
    dec     eax
    UNEQUAL_ROWS:
    mov     ecx, eax
    inc     ecx    
    
    mov     esi, pArray
    NEXT_ROW:
        push    esi
        push    ecx
        
        ;Inner loop outputs th values in the current row
        mov     ecx, NUM_COLUMNS
        NEXT_ELEMENT:
            ;Output values separated by a tab character
            mov     eax, [esi]
            call    WriteDec

            lea     edx, tabChar
            call    WriteString
            
            inc     outCount
            
            ;Compute address of next value for output
            add     esi, rowOffset
            
            ;When the number of elements is not evenly divisible by 10, there will
            ;be a partial row.  For consecutive output, the first x values output
            ;must be incremented to the next array index (where x is the remainder
            ;of elements that were not evenly divisible by 10).  This will 
            ;compensate for the extra row in that column.
            mov     eax, NUM_COLUMNS
            sub     eax, ecx
            inc     eax
            cmp     eax, extraRow
            jg      NO_EXTRA
            
            add     esi, DWORD_BYTES
            
            NO_EXTRA:
            mov     eax, pQtyElements
            cmp     outCount, eax       ;If outCount > pQtyElements, terminate the row output
            jl      CONTINUE_LOOP
            
            mov     ecx, 1              ;Set loop drive to termination condition
            
            CONTINUE_LOOP:
            loop    NEXT_ELEMENT

        pop     ecx     ;Restore outer loop counter
        pop     esi     ;Restore last row header address

        ;Move to next index to start the next row
        add     esi, DWORD_BYTES
        loop    NEXT_ROW        

    call    CrLf
    call    CrLf

    ret 12
displayList     ENDP

sortArray       PROC    USES eax ebx ecx edx,   ;sort list
    pArray:PTR DWORD,
    pQtyElements:DWORD
;   DESCRIPTION:    A heap sort function that receives an unsorted array,
;                   builds it into a heap array, and sort the array in
;                   descending value.
;   RECEIVES:       pArray - Pointer to an Array (contains at least one element)
;                   pQtyElements - The size of the array
;   RETURNS:        NONE! However, pArray is modified by this procedure as it 
;                   is passed by reference.
    ;Set loop driver to (array-size / 2)
    mov     eax, pQtyElements
    mov     ebx, 2
    cdq
    div     ebx
    mov     ecx, eax

    ;Set highest index in array to eax
    mov     eax, pQtyElements
    dec     eax

    ;Transforms array into a heap array
    ADJUST_NEXT:
        mov     edx, ecx
        dec     edx
        
        push    edx         ;Current Index to be adjusted
        push    eax         ;Highest index to check, exclusively
        push    pArray
        call    adjustHeap

        loop    ADJUST_NEXT
    
    ;Sorts heap array into descending value
    mov     eax, 0              ;0th element is always the smallest in a heap array!
    mov     ecx, pQtyElements   ;Loop driver is size of the array
    SORT_NEXT:
        mov     edx, ecx
        dec     edx             ;Last element for swap is always 1 less than loop driver
        
        ;Swap values in array
        push    edx
        push    eax
        push    pArray
        call    swapElements

        dec     edx             ;We don't want to include the element we just swapped!
        
        ;Adjust Heap post-swap to maintain heap array
        push    eax
        push    edx
        push    pArray
        call    adjustHeap
        
        loop    SORT_NEXT
            
    ret 8
sortArray       ENDP

adjustHeap      PROC    USES eax ebx edx esi,
    pArray:PTR DWORD,
    pMax:DWORD,
    pPos:DWORD
;   DESCRIPTION:    Supporting function to sortArray.  Percolates value at pPos
;                   down the array - percolation occurs in a heap fashion.   
;   RECEIVES:       pArray - Pointer to an Array (contains at least one element)
;                   pMax -   Index of greatest element in array that can be
;                            adjusted.  Think of it as a capacity for testable
;                            elements in the array.
;                   pPos -   Index of element in array to be conditionally 
;                            adjusted.
;   RETURNS:        NONE! However, pArray is modified by this procedure as it 
;                   is passed by reference.
    LOCAL   smallerChild:DWORD, 
            rightChild:DWORD, 
            leftChild:DWORD,
            tempVal:DWORD

    ;Initialize local variables leftChild and rightChild of pPos
    mov     eax, pPos
    mov     ebx, 2
    mul     ebx
    inc     eax
    mov     leftChild, eax      ;2 * pos + 1
    inc     eax
    mov     rightChild, eax     ;2 * pos + 2

    ;Determine number of children within range of pMax
    cmp     eax, pMax
    jg      ONE_CHILD
    
    ;Determine the smaller of both children to pPos
    lea     eax, smallerChild
    push    eax
    push    rightChild
    push    leftChild
    push    pArray
    call    smallerIndex
    
    ;Compare values stored at smallerChild and pPos
    mov     esi, pArray
    mov     eax, pPos
    mov     ebx, DWORD_BYTES
    mul     ebx
    add     esi, eax
    mov     eax, [esi]          ;value at pPos
    mov     tempVal, eax 

    mov     esi, pArray
    mov     eax, smallerChild
    mul     ebx
    add     esi, eax            ;value at smallerChild
    mov     eax, [esi]          
    
    cmp     eax, tempVal
    jge     END_ADJUSTHEAP      ;Only swap if value at smallerChild < value at pPos

    ;Swap values in array
    push    smallerChild
    push    pPos
    push    pArray
    call    swapElements
    
    ;Recursive call to adjust heap of smallerChild (index, not value)
    ;This will percolate the swapped values appropriately
    push    smallerChild
    push    pMax
    push    pArray
    call    adjustHeap
    jmp     END_ADJUSTHEAP

    ONE_CHILD:
    mov     eax, leftChild
    cmp     eax, pMax           ;Ensure pPos only has one child, otherwise end procedure
    jg      END_ADJUSTHEAP

    ;Compare values stored at leftChild and pPos
    mov     esi, pArray
    mov     eax, pPos
    mov     ebx, DWORD_BYTES
    mul     ebx
    add     esi, eax
    mov     eax, [esi]          ;value at pPos
    mov     tempVal, eax       

    mov     esi, pArray
    mov     eax, leftChild
    mov     ebx, DWORD_BYTES
    mul     ebx
    add     esi, eax            ;value at leftChild
    mov     eax, [esi]
    
    cmp     eax, tempVal
    jge     END_ADJUSTHEAP         ;Only swap if value at leftChild < than value at pPos

    ;Swap values in array
    push    leftChild
    push    pPos
    push    pArray
    call    swapElements
    
    ;Recursive call to adjust heap of leftChild (index, not value)
    ;This will percolate the swapped values appropriately
    push    leftChild
    push    pMax
    push    pArray
    call    adjustHeap
    
    END_ADJUSTHEAP:
    ret 12
adjustHeap      ENDP

swapElements    PROC    USES eax ebx edx esi,
    pArray:PTR DWORD,
    pIndex1:DWORD,
    pIndex2:DWORD
;   DESCRIPTION:    A supporting procedure for the sortArray procedure.
;                   This procedure swaps the values stored at two indexes of an
;                   array passed to this function.
;   RECEIVES:       pArray - A pointer to an array with at least two elements.
;                   pIndex1 - The first index of an element in the array.
;                   pIndex2 - The second index of an element in the array.
;   RETURNS:        NONE! However, pArray is modified by this procedure as it 
;                   is passed by reference.

    ;Retrieve value stored at pIndex1 and push to stack
    mov     esi, pArray
    mov     eax, pIndex1
    mov     ebx, DWORD_BYTES
    mul     ebx
    add     esi, eax
    mov     eax, [esi]
    push    eax
    
    ;Retrieve value stored at pIndex2 and push to stack
    mov     esi, pArray
    mov     eax, pIndex2
    mul     ebx
    add     esi, eax
    mov     eax, [esi]
    push    eax

    ;Pop stack containing value at pIndex2 to index of pIndex1
    mov     esi, pArray
    mov     eax, pIndex1
    mov     ebx, DWORD_BYTES
    mul     ebx
    add     esi, eax
    pop     eax
    mov     [esi], eax

    ;Pop stack containing value at pIndex1 to index of pIndex2
    mov     esi, pArray
    mov     eax, pIndex2
    mov     ebx, DWORD_BYTES
    mul     ebx
    add     esi, eax
    pop     eax
    mov     [esi], eax

    ret 12
swapElements    ENDP

smallerIndex    PROC    USES eax ebx edx esi,
    pArray:PTR DWORD,
    pLeft:DWORD,
    pRight:DWORD,
    pSmaller:PTR DWORD
;   DESCRIPTION:    A supporting procedure for the sortArray procedure.
;                   Determines the lesser value between two indices and returns
;                   the index of the value.
;   RECEIVES:       pArray -   A pointer to an array with at least two elements.
;                   pRight -   The index of an element in the array.  Directional
;                              importance of variable name is trivial.
;                   pLeft -    The index of an element in the array.  Directional
;                              importance of variable name is trivial.
;                   pSmaller - Variable reference pointer for returning the index
;                              value determined by this procedure.
;   RETURNS:        pSmaller - This reference variable will contain the index 
;                              value of the lesser value stored in pRight and 
;                              pLeft.

    ;Retrieve value stored at pLeft and push to the stack
    mov     esi, pArray
    mov     eax, pLeft
    mov     ebx, DWORD_BYTES
    mul     ebx
    add     esi, eax
    mov     eax, [esi]
    push    eax

    ;Retrieve value stored at pRight
    mov     esi, pArray
    mov     eax, pRight
    mul     ebx
    add     esi, eax
    mov     ebx, [esi]
    
    pop     eax
    cmp     eax, ebx        ;Compares values stored at pLeft and pRight, respectively
    jl      LEFT_SMALLER

    ;Return pRight
    mov     esi, pSmaller
    mov     eax, pRight
    mov     [esi], eax
    jmp     END_SMALLERINDEX

    LEFT_SMALLER:
    ;Return pLeft
    mov     esi, pSmaller
    mov     eax, pLeft
    mov     [esi], eax
    
    END_SMALLERINDEX:
    
    ret 16
smallerIndex    ENDP

dispMedian      PROC    USES eax ebx edx edi,  ;display median
    pArray:PTR DWORD,
    pQtyElements:DWORD,
    pMedianMsg:PTR BYTE,
    pFMedianMsg:PTR BYTE,
    pPeriodCrLf:PTR BYTE
;   DESCRIPTION:    Displays the median of an array of values.
;   RECEIVES:       pArray -   A pointer to an array with at least two elements.
;                   pQtyElements - The size of the array
;                   pMedianMsg - The first section of output that describes the output.
;                   pPeriodCrLf - The ending of output, a period and line break
;   RETURNS:        NONE.
    LOCAL   median:DWORD

    FINIT   ;Initialize FPU

    ;Initialize median and ecx register to zero
    mov     median, 0
    mov     ecx, 0

    ;Determine memory index of element: (max / 2)
    mov     eax, pQtyElements
    mov     ebx, 2
    cdq
    div     ebx
    mov     ebx, DWORD_BYTES
    push    edx                 ;This is a flag for a conditional later in procedure
    mul     ebx
    pop     edx                 ;Restore flag after 'mul' in the event it uses edx
    push    eax                 ;Store the memory index of (max / 2)

    ;Determine memory index of element: (max / 2) - 1
    sub     eax, ebx
    
    cmp     edx, 0              ;If (max / 2) had no remainder, then a mean of middle values is required
    jne     NO_MEAN1

    ;Retrieve value at (max / 2) - 1 and store in ecx
    mov     esi, pArray
    add     esi, eax
    mov     eax, [esi]
    mov     ecx, eax
    
    ;Load the first operand to the FPU stack
    fild    dword ptr [esi]
    
    NO_MEAN1:
    ;Retrieve value at (max / 2) and add to ecx
    mov     esi, pArray
    pop     eax
    add     esi, eax
    add     ecx, [esi]          ;add instead of mov in the event of mean of middle values
    
    ;Load the second operand to the FPU stack
    fild    dword ptr [esi]
    
    cmp     edx, 0              ;If (max / 2) had no remainder, then a mean of middle values is required
    jne     NO_MEAN2
    
    ;Compute the mean of middle values
    mov     eax, ecx
    mov     ebx, 2
    cdq
    div     ebx
    cmp     edx, 0
    je      NO_ROUND
    inc     eax
    NO_ROUND:
    mov     ecx, eax

    ;Add operands on FPU stack
    fadd

    ;Load the divisor, 2, to the FPU stack
    push    ebx
    fild    dword ptr [esp]
    pop     ebx
    
    ;Divide operands on FPU stack
    fdiv

    NO_MEAN2:
    mov     median, ecx
    fist    median          ;This decrements the value stored in median
    mov     median, ecx     ;This correctes the decrement from fist operation

    ;Output Median as Integer
    mov     edx, pMedianMsg
    call    WriteString

    mov     eax, median
    call    WriteDec

    mov     edx, pPeriodCrLf
    call    WriteString

    ;Output Median as Floating point
    mov     edx, pFMedianMsg
    call    WriteString

    call    WriteFloat     
    
    mov     edx, pPeriodCrLf
    call    WriteString

    ret 16
dispMedian      ENDP

END main