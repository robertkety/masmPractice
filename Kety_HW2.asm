TITLE Fibonacci Number Generator						(Kety_HW2.asm)

; Author: Robert Kety
; Modified: 06/23/2014
; Description:		Outputs a quantity of Fibonacci numbers based upon user 
;				input.  The quantity range will be 1 to 46 Fibonacci 
;				numbers (inclusive).

INCLUDE Irvine32.inc

;Constants
USERNAME_LEN	= 32		;Length of username including null pointer
MAX_FIB_ARRAY	= 46		;The maximum number of Fibonnaci numbers available to 
					;user including null pointer
COL_TAB		= 5		;Tabs between columns are 5 spaces
COL_DIGITS	= 10		;The maximum number of decimal digits in each column 
					;based on 16^8 (DWORD)
COL_NUM		= 5		;The maximum number of columns per row

.data
;Variables - In order by use
intro1		BYTE		"Welcome to the Fibonacci Number Generator!", 0dh, 0ah
			BYTE		"  Programmed by Robert Kety (Homework 2)", 0dh, 0ah
			BYTE		0
prompt1		BYTE		"Please enter your name: ", 0
userName		BYTE		33 DUP(0)		;USERNAME_LEN + 1 for Null pointer
intro2		BYTE		"Nice to meet you, ", 0
intro3		BYTE		"How many Fibonacci numbers would you like to see tod"
			BYTE		"ay? [1 - 46]: ", 0
invalidQty	BYTE		"That number is out of range.  Please enter an intege"
			BYTE		"r from 1 to 46: ", 0
fibQty		DWORD	?
fibArray		DWORD	47 DUP(0)		;MAX_FIB_ARRAY + 1 for Null pointer
space		BYTE		" ", 0
farewell		BYTE		"Goodbye, ", 0
exclamation	BYTE		"!", 0dh, 0ah, 0dh, 0ah, 0

.code
main PROC
	call		Clrscr  

	;Output Title and Author Introduction
	mov		edx, OFFSET intro1
	call		WriteString
	call		CrLf

	;Prompt for user name, receive user name, and greet user
	mov		edx, OFFSET prompt1
	call		WriteString
	
	mov		edx, OFFSET userName
	mov		ecx, USERNAME_LEN
	call		ReadString
	call		CrLf

	mov		edx, OFFSET intro2
	call		WriteString
	mov		edx, OFFSET userName
	call		WriteString
	call		CrLf
	call		CrLf

	;Get valid quantity of Fibonacci numbers from user and store in fibQty
	call		GetQty
	mov		fibQty, eax
	
	;Compute and store Fibonacci numbers
	call		GetFib
		
	;Output fibarray to user
	call		OutputArray
	
	;Extra white space for readability
	call		CrLf
	call		CrLf

	;Goodbye!
	mov		edx, OFFSET farewell
	call		WriteString
	mov		edx, OFFSET userName
	call		WriteString
	mov		edx, OFFSET exclamation
	call		WriteString

	exit
main ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
GetQty PROC	USES edx
;	Prompts user for a quantity of Fibonacci numbers.  The number must be
;	1 to FIB_MAX_ARRAY, inclusive.  Invalid response results in another prompt
;	requesting a valid number.
;	Receives:	None
;	Returns:	EAX, the valid uantity of Fibonacci numbers from 
;			[1 - FIB_MAX_ARRAY]
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	;Prompt user for valid quantity of Fibonacci numbers
	mov		edx, OFFSET intro3
	call		WriteString
	
	;Read input and post-test for validity
	ASK_QTY:			
			call		ReadInt
			cmp		eax, 0
			jle		INVALID_QTY			;Cannot be <= 0
			cmp		eax, MAX_FIB_ARRAY
			jg		INVALID_QTY			;Cannot be > FIB_MAX_ARRAY
			jmp		VALID_QTY
	INVALID_QTY:	
			mov		edx, OFFSET invalidQty	;Prompt user again
			call		WriteString
			jmp		ASK_QTY				
	VALID_QTY:
			call		CrLf					;extra space for readability
			ret			
GetQty ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
IterFibo PROC USES ebx ecx
;	Returns the nth Fibonnaci number.  
;	Note:	Zero (0) is not considered a valid Fibonacci number in this
;			program.
;	Receives:	ECX, the n desired (1 to FIB_MAX_ARRAY)
;	Returns:	EAX, the nth Fibonnaci number
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	;Set initial sum values
	mov		eax, 0
	mov		ebx, 1

	cmp		ecx, 0
	jle		ZERO_N	;If n == 0, return 0
	
	L1:
			add		eax, ebx
			push		ecx			; Store loop count during register swap
			mov		ecx, eax
			mov		eax, ebx
			mov		ebx, ecx
			pop		ecx			; Restore loop count
			loop		L1
	
	jmp		FINAL_FIB

	ZERO_N:
			mov		eax, 0
			jmp		FINAL_FIB

	FINAL_FIB:
			ret
IterFibo ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
GetFib PROC USES eax ecx esi
;	Compute and store fibQty number of Fibonacci numbers.
;	Note:	Numbers are pushed on to the EAX register so they can pop 
;			in ascending order when assigned to fibArray
;	Requires:	fibQty, greater than 0
;	Returns:	None, values are stored in fibArray
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

	;Compute fibQty of Fibonacci numbers
	mov		ecx, fibQty
	L1:
			call		IterFibo
			push		eax
			loop		L1
	
	;Assign fibQty of Fibonacci numbers to fibArray in ascending order
	mov		esi, OFFSET fibArray
	mov		ecx, fibQty
	L2:
			pop		eax
			mov		[esi], eax
			add		esi, TYPE DWORD
			loop		L2
	ret
GetFib ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
ColumnSpaces PROC USES eax ebx ecx edx
;	Determine number of spaces required to reach the right-edge of column.
;	Column width is COL_DIGITS.  The number of digits in EAX (decimal) plus
;	the number of spaces generated in this procedure will be COL_DIGITS.
;	Requires:	EAX, a number with 1 - 10 digits
;			AddSpaces Procedure
;	Returns:	None, Output to user only
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	;Store current Fibonacci number in EDX register and push for storage until
	;needed to determine number of spaces and placement of CrLf.
	mov		edx, eax
	push		edx

	;Each fibonacci is tested against powers of ten to determine the number of
	;spaces required to make a column 10 digits wide. Testing begins with 10.
	mov		ebx, 10
	
	;The following is a loop for determining the number of spaces required to
	;make a column 10 digits wide.  There are several jumps, but the jumps do
	;not step out of the loop.
	mov		ecx, COL_DIGITS
	SUM_DIGITS:
			cdq				;EDX contains a value upon looping
			div		ebx
			cmp		eax, 0
			jle		EXIT_LOOP		;if number of digits is less than 2
			jg		TEST_POWER	;else, test next power of ten
			EXIT_LOOP:
					mov		eax, ecx		;Final number of spaces
					mov		ecx, 1		;Set loop to exit value
					jmp		END_OF_LOOP
			TEST_POWER:
					cmp		ebx, 1000000000
					jl		NEXT_POWER	;if next power of ten will
										;not cause overflow
					;mov		ecx, 1		;else, set loop to exit value
					jmp		END_OF_LOOP

			NEXT_POWER:		
					;Move to next power of ten
					mov		eax, COL_DIGITS
					mul		ebx
					mov		ebx, eax
						
					;Recall last Fibonacci number to EAX and push to EDX
					pop		edx
					mov		eax, edx
					push		edx
			END_OF_LOOP:			
					loop	SUM_DIGITS
			
	;Insert the number of spaces necessary to make columns 10 digits wide.
	call		AddSpaces
	
	;Insert CrLf after every 5th Fibonacci number.
	;Note:	Every 5th Fibonacci number is divisible by 5		
	pop		edx			;Pop last Fibonacci number to clear EDX memory
	mov		eax, edx
	cdq					;Clear EDX for division
	mov		ebx, 5
	div		ebx
	cmp		edx, 0		
	je		NEXT_LINE		;If Fibonacci % 5 == 0

	mov		eax, COL_TAB	;else, insert 5 spaces
	call		AddSpaces
	jmp		END_PROC
			
	NEXT_LINE:
			call		CrLf
			
	END_PROC:
			ret

ColumnSpaces ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
AddSpaces PROC USES eax ecx edx
;	Outputs spaces equivalent to the number passed in the EAX register
;	Requires:	EAX, the number of spaces to output
;	Returns:	None, Output to user only
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	mov		ecx, eax
	mov		edx, OFFSET space
	TabLoop:
			cmp		ecx, 0
			je		END_LOOP
			call		WriteString
			loop		TabLoop	
	END_LOOP:
			ret

AddSpaces ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
OutputArray PROC
;	Output array of Fibonacci numbers in fibArray.
;	Note:	Assignment requires 5 columns of Fibonacci numbers per line.
;	Requires:	fibQty,	greater than 0
;			fibArray,	with fibQty number of elements
;			ColumnSpaces procedure
;	Returns:	None, output to user only
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	mov		esi, OFFSET fibArray
	mov		ecx, fibQty
	L1:
			;Write next Fibonacci number
			mov		eax, [esi]
			add		esi, TYPE DWORD
			call		WriteDec
			
			;Insert appropriate spaces between columns to separate and 
			;justify for readability.  Also, move to next line after 5
			;columns of Fibonacci numbers
			call		ColumnSpaces

			loop		L1
	ret
OutputArray ENDP

END main