TITLE Homework 1			(Kety_HW1.asm)

; Author: Robert Kety
; Modified: 06/23/2014
; Description:  This program computes elementary aritmethic of two integers
;				including addition, subtraction, multiplication, division
;				(integer and decimal).
;				Extra credit modifications include looping the program,
;				validation of first and second integers, and the calculation
;				and display of floating-point, rounded to the thousandth
;				place, for division.  The determination of "something
;				astoundingly creative" I leave to the instructor and TA's.

INCLUDE Irvine32.inc

.data								;No order stated in requirements. Order is by use, not type

intro_1		BYTE	"Elementary Arithmetic by Robert Kety (Homework 1)", 0
intro_2		BYTE	"Enter 2 integers, and I'll show you the sum, difference", 0
intro_3		BYTE	"product, quotient, and remainder.", 0
prompt_1	BYTE	"Please enter first integer: ", 0
negInput_1	BYTE	"First Integer may not be negative.  Please try again.", 0
integer_1	DWORD	?				;integer to be entered by user
prompt_2	BYTE	"Please enter second integer: ", 0
negInput_2	BYTE	"Second integer must be greater than first integer.  Please try again.", 0
negInput_3	BYTE	"Second integer may not be less than or equal to zero.  Please try again.", 0
integer_2	DWORD	?				;integer to be entered by user
plus		BYTE	" + ", 0
minus		BYTE	" - ", 0
multiply	BYTE	" * ", 0
divide		BYTE	" / ", 0
equals		BYTE	" = ", 0
sum			DWORD	?				;sum of two integers
difference	DWORD	?				;difference of two integers
product		DWORD	?				;product of two integers
quotient	DWORD	?				;quotient in division of two integers
remainder	DWORD	?				;remainder in division of two integers
tensPlace	DWORD	0				;Used in the decimal point calculation of division to the thousandth place
hundPlace	DWORD	0				;Used in the decimal point calculation of division to the thousandth place
thouPlace	DWORD	0				;Used in the decimal point calculation of division to the thousandth place
tempRemain	DWORD	0				;Temporary memory location for transferring remainders from edx to eax registers
remString	BYTE	" remainder ", 0
orString	BYTE	" or ", 0
decPoint	BYTE	".", 0
runAgain	BYTE	"Would you like to run another set of integers (y/n)? ", 0
userResp	BYTE	17 DUP(0)		;User response to runAgain
yesLower	DWORD	121d			;ASCII decimal for 'y' character
farewell	BYTE	"Until next time!", 0

.code
main PROC
	call Clrscr
	
;Introduce title of program and programmer
	mov		edx, OFFSET intro_1
	call	WriteString
	call	CrLf
	call	CrLf					;Additional white space for readability

;Provide introductory instructions to user
	mov		edx, OFFSET intro_2		;Line 1 of instructions
	call	WriteString
	call	CrLf

	mov		edx, OFFSET intro_3		;Line 2 of instructions
	call	WriteString 
	call	CrLf
	
;Prompt user for two integers
	EnterInt_1:						;Get first integer from user
		;Reset variables to zero - Necessary for successive iterations of procedure
		mov		tensPlace, 0
		mov		hundPlace, 0
		mov		thouPlace, 0

		call	CrLf				;Additional white space for readability
		mov		edx, OFFSET prompt_1;Prompt user for first integer
		call	WriteString
		
		call	ReadInt				;Receive first integer to eax register
		cmp		eax, 0
		jl		NegativeWarning_1	;if (eax < 0), then output NegativeWarning_1
		jmp		Continue_1			;else continue main proc
	
		NegativeWarning_1:			;Warns user if first integer is negative and
									;jumps to prompt for a new first integer
			mov		edx, OFFSET negInput_1
			call	WriteString
			call	CrLf
			call	CrLf			;Additional white space for readability
			jmp		EnterInt_1

		Continue_1:
		mov		integer_1, eax		;Store first integer at memory location (integer_1)

	EnterInt_2:						;Get second integer from user
		mov		edx, OFFSET prompt_2;Prompt user for second integer
		call	WriteString
		
		call	ReadInt				;Receive second integer to eax register	
		cmp		eax, integer_1
		jg		NegativeWarning_2	;if (eax > integer_1), then output NegativeWarning_2
	
		cmp		eax, 0					
		jle		NegativeWarning_3	;else-if (eax <= 0), then output NegativeWarning_3
		jmp		Continue_2			;else continue main proc

		NegativeWarning_2:			;Warns user if second integer is less than first integer
									;jumps to prompt for a new second integer
			mov		edx, OFFSET negInput_2
			call	WriteString
			call	CrLf
			call	CrLf			;Additional white space for readability
			jmp		EnterInt_2

		NegativeWarning_3:			;Warns user if second integer is zero
									;jumps to prompt for a new second integer
			mov		edx, OFFSET negInput_3
			call	WriteString
			call	CrLf
			call	CrLf			;Additional white space for readability
			jmp		EnterInt_2

		Continue_2:
		mov		integer_2, eax		;Store second integer at memory location (integer_2)
		call	CrLf				;Additional white space for readability

;Perform calculations
	;Add
		mov		eax, integer_1
		add		eax, integer_2
		mov		sum, eax

	;Subtract
		mov		eax, integer_1
		sub		eax, integer_2
		mov		difference, eax

	;Multiply
		mov		eax, integer_1
		mov		ebx, integer_2
		mul		ebx
		mov		product, eax

	;Divide
		mov		eax, integer_1
		cdq
		mov		ebx, integer_2
		div		ebx
		mov		quotient, eax
		mov		remainder, edx

		;Remainder to Decimal Point (to thousandth)
		;Decimal point remainder is stored as three integers
			cmp		remainder, 0
			je		Output			;if(remainder == 0), jump to output

			mov		eax, remainder	;remainder from integer_1 / integer_2
		mov		ebx, 10
		mul		ebx
		cdq
		mov		ebx, integer_2
		div		ebx
		mov		tensPlace, eax		;tensPlace == (remainder * 10) / integer_2
	
		cmp		edx, 0				;if(remainder == 0), jump to output
		je		Output

		mov		tempRemain, edx		;remainder from tensPlace division
		mov		eax, tempRemain
		mov		ebx, 10
		mul		ebx
		cdq
		mov		ebx, integer_2
		div		ebx
		mov		hundPlace, eax		;hundPlace == (remainder * 10) / integer_2
	
		cmp		edx, 0				;if(remainder == 0), jump to output
		je		Output

		mov		tempRemain, edx		;remainder from hundPlace division
		mov		eax, tempRemain
		mov		ebx, 10
		mul		ebx
		cdq
		mov		ebx, integer_2
		div		ebx
		mov		thouPlace, eax		;thouPlace == (remainder * 10) / integer_2
	
		cmp		edx, 0				;if(remainder == 0), jump to output
		je		Output
	
		mov		tempRemain, edx		;remainder from thouPlace division
		mov		eax, tempRemain
		mov		ebx, 10
		mul		ebx
		cdq
		mov		ebx, integer_2
		div		ebx					;ten-thousandth place is computed to round the thousandth place
		
		cmp		eax, 5
		jl		Output				;if(ten-thousandth place < 5), jump to output (i.e., round down)
		inc		thouPlace			;else increment thouPlace (i.e., round up)
		
;Output Results of Calculation
	Output:								
		;Addition of two integers
			mov		eax, integer_1
			call	WriteDec

			mov		edx, OFFSET plus
			call	WriteString

			mov		eax, integer_2
			call	WriteDec

			mov		edx, OFFSET equals
			call	WriteString

			mov		eax, sum
			call	WriteDec
			call	CrLf

		;Subtraction of two integers
			mov		eax, integer_1
			call	WriteDec

			mov		edx, OFFSET minus
			call	WriteString
				
			mov		eax, integer_2
			call	WriteDec

			mov		edx, OFFSET equals
			call	WriteString

			mov		eax, difference
			call	WriteDec
			call	CrLf

		;Multiplication of two integers
			mov		eax, integer_1
			call	WriteDec

			mov		edx, OFFSET multiply
			call	WriteString

			mov		eax, integer_2
			call	WriteDec

			mov		edx, OFFSET equals
			call	WriteString

			mov		eax, product
			call	WriteDec
			call	CrLf

		;Division of two integers
			mov		eax, integer_1
			call	WriteDec

			mov		edx, OFFSET divide
			call	WriteString
	
			mov		eax, integer_2
			call	WriteDec

			mov		edx, OFFSET equals
			call	WriteString

			mov		eax, quotient
			call	WriteDec
			mov		edx, OFFSET remString	
			call	WriteString
			mov		eax, remainder
			call	WriteDec
		
			;Additional information on decimal place conversion
				mov		edx, OFFSET orString
				call	WriteString
				mov		eax, quotient
				call	WriteDec
				mov		edx, OFFSET decPoint
				call	WriteString

				mov		eax, tensPlace
				call	WriteDec
				mov		eax, hundPlace
				call	WriteDec
				mov		eax, thouPlace
				call	WriteDec	

				call	CrLf
				call	CrLf		;Additional white space for readability

;Prompt user for another calculation
	mov		edx, OFFSET runAgain
	call	WriteString

	mov		edx, OFFSET userResp	;Read users response
	mov		ecx, 16					;Ensures enough room in BYTE array for additional characters
	call	ReadString

	mov		eax, yesLower
	cmp		al, [userResp]			;BYTE is 8-bit
	je		EnterInt_1				;if(userResp[0] == 'y'), then run again
	sub		eax, 32d				;Adjust DWORD value for 'y' to 'Y'
	cmp		al, [userResp]			;BYTE is 8-bit
	je		EnterInt_1				;if(userResp[0] == 'Y'), then run again


;Say "Good-bye"
	call	CrLf
	mov		edx, OFFSET farewell
	call	WriteString
	call	CrLf
	call	CrLf					;Additional white space for readability
	
	exit		; exit to operating system
main ENDP

END main