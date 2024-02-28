TITLE Final Project	(main.asm)
; Description:    Final Project - John Conway's Game of Life. Our zero-player game outputs to the terminal a grid 
;						    (size, color, and starting pattern based on user input), and prints the generations of the cells
;						    based on the rules of the game (see requirements specification).
; Authors: Jonathan Hamstra, Emma Foulk, Shane Modena
; Sources: John Conway, Game of Life
;		 Kip R. Irvine - Assembly Language for x86 Processors Textbook
; Date: 5/18/21

INCLUDE Irvine32.inc

GetProcessHeap PROTO

.data

dim DWORD 10					;dimension of the grid (default: 10x10)
line BYTE 0					;tracks which line we are printing for x/y cursor locatoin
liveSquareColor DWORD 14			;variable to set color based on user input (default = yellow)

;PROMPTS PRINTED TO SCREEN FOR USER INPUT

prompt BYTE "Welcome to John Conway's GAME OF LIFE!!!     " ,0
prompt2 BYTE "Enter the dimension of your grid ('10' will create a 10x10 grid): " ,0
prompt3 BYTE "Enter an integer to change the color of the cells (some of our favorites are 1-15 or 210-223): " ,0
prompt4 BYTE "Enter an integer to choose a pattern (1 - Glider, 2 - Flasher, 3 - Bomb): " ,0

hHeap HANDLE ?					;used to allocate memory from the heap

ARRAY_SIZE DWORD 800			; number of bytes in two arrays (default: two 10x10 DWORD grids is 800 bytes)
BoredSize DWORD 100				; number of DWORDS in one board (10x10 default grid)
bored_size_bytes DWORD 400		; number of BYTES in one board (10x10 default grid)

neighbors DWORD ?				;used to track the number of live cells neighboring the cell in question



;Variables that identify surrounding squares, default is 10x10 grid

uLeft DWORD 44					;"upper" surrounding squares
uMiddle DWORD 40
uRight DWORD 36

cLeft DWORD 4					;left and right squares, cMiddle is the square in question
cMiddle DWORD 0
cRight DWORD 4

bLeft DWORD 36					;"bottom" surrounding squares
bMiddle DWORD 40
bRight DWORD 44



LeftCornerNumber DWORD ?			;used in checkSquare, finds the index value for the bottom left corner (ex: in 10x10 grid, LeftCornerNumber = 91)

pbored DWORD ?		;yes, it's bored			present array
fbored DWORD ?		;yes, it's also bored		future array





.code
main proc

call userInput						;gets user input of grid size and color

call make_heap						;dynamically allocate memory for two arrays
call allocate_array_present

call patterns						;user input for which pattern (glider, flasher, bomb)
call displayBoard					;displays initial pattern

call game_loop						;loops through 25 generations of the pattern and displays each to the terminal



mov eax, 15						;resets color so end notifications look normal
call SetTextColor



	exit
main ENDP



;__________________________________________________________________________________________
; make_heap
;	Finds address for program's existing heap area, will be used in allocate_array_present.
; 
; Returns: EAX, placed into hHeap, holding heap memory address
;__________________________________________________________________________________________

make_heap PROC
	INVOKE GetProcessHeap						;returns 32-bit integer handle to program's existing heap area in EAX (if failure, EAX = NULL)
	.IF eax == NULL
		call WriteWindowsMsg					;error while creating heap, display error message
		jmp quit
	.ELSE
		mov hHeap, eax							;move heap handle into hHeap
	.ENDIF
	quit:
ret
make_heap ENDP


;__________________________________________________________________________________________
; allocate_array_present
;	Allocates two grid's worth of memory from the heap and fills them with zeros. Stores
;	address to beginning of memory in pbored, middle of memory in fbored.
; 
; Receives: hHeap (program's existing heap area)
; Returns: pbored - holds address to beginning of allocated memory
;		 fbored - holds address to middle of allocated memory, acts as second "array"
;__________________________________________________________________________________________

allocate_array_present PROC USES eax ecx ebx
	INVOKE HeapAlloc, hHeap, HEAP_ZERO_MEMORY, ARRAY_SIZE		;allocates a block of memory from a heap, size based on user input, return value in EAX is address of memory block, fills all of memory with 0s
	.IF eax == NULL				;if memory wasn't allocated
		call WriteWindowsMsg		
	.ELSE
		mov pbored, eax			;pbored holds memory location for dynamically allocated "arrays"
	.ENDIF

	;mov eax, ARRAY_SIZE			;filling board with 0s, unnecessary because of "HEAP_ZERO_MEMORY"	
	;mov ecx, 4					;could fill board with other values, good for debugging
	;div ecx
	;mov ARRAY_SIZE, eax

	;mov esi, pbored			
	;mov ecx, ARRAY_SIZE			
	;mov ebx, 0
	;L3:
	;	mov [esi], ebx
	;	add esi, 4
	;loop L3


	mov esi, pbored				;sets esi to beginning of memory
	add esi, bored_size_bytes		;finds memory location half way into dynamically allocated memory (where the necxt "array" should start)
	mov fbored, esi				;sets fbored to the middle location


ret
allocate_array_present ENDP





;__________________________________________________________________________________________
; userInput
;	Prompts user to input grid size and the color of live squares.
; 
; Receives: user input using ReadDec
; Returns: dim - fills with user inputed dimension of grid (ex: "10" is a 10x10 grid)
;		 BoredSize - fills with number of "entries" in grid based on user input.
;		 bored_size_bytes - fills with number of bytes in a single "array"
;		 LiveSquareColor - user inputed color (integer)
;__________________________________________________________________________________________

userInput PROC
	mov edx, OFFSET prompt
	call WriteString			;this introduces the user to the game

	mov edx, OFFSET prompt2
	call WriteString			;this asks the user for input

	call ReadDec				;grabs user input
	mov dim, eax

	mul dim					;setting array size based on user input
	mov ebx, 8				;multiply by 4 (to get number of bytes), multiply by 2 (two arrays in dynamic allocation), 2*4=8
	mul ebx
	mov array_size, eax			;this gives us the total number of bytes in the entire allocated heap

	mov eax, dim
	mul dim
	mov BoredSize, eax			;this gives us the size of a standard board in literal squares, used for loop counters
	mov edx, 4
	mul edx
	mov bored_size_bytes, eax	;this is the number of a single board in bytes, used later for different loop counters


	;setting surrounding grid variables

	mov edx, 4
	
	mov eax, dim
	inc eax
	mul edx
	mov uLeft, eax
	mov bRight, eax

	mov edx, 4

	mov eax, dim
	dec eax
	mul edx
	mov uRight, eax
	mov bLeft, eax

	mov edx, 4

	mov eax, dim
	mul edx
	mov uMiddle, eax
	mov bMiddle, eax

	mov edx, OFFSET prompt3
	call WriteString				;asks user for what color they want the alive squares to be

	call ReadDec
	mov liveSquareColor, eax			;222 looks cool


ret
userInput ENDP





;__________________________________________________________________________________________
; displayBoard
;	Loops through the size of the board (in rows/columns) and prints each line (calls on printLine).
; 
; Receives: pbored - location of array to be printed
; Returns: (prints to terminal)
;__________________________________________________________________________________________


displayBoard PROC uses ecx

	mov line, 0

	mov esi, pbored				;change this line from fbored/pbored to switch which is outputting
	mov ecx, dim					;loops through based on the dimension of the grid - dim equals number of rows/columns
L1:
	mov dx, 0						;goes to the leftmost square, at the vertical line indicated by the "line" variable
	mov dh, line
	call Gotoxy
	call printLine					;once set up, sends it to printline to actually finish the line
	add line, 1
loop L1

ret
displayBoard ENDP


;__________________________________________________________________________________________
; printLine
;	Sets color of the live square, prints out a line of spaces (0/black if dead, 1/color if alive).
;	One line of spaces is the length of the dimension of the board.
; 
; Receives: liveSquareColor - user integer for color of live square
; Returns: (prints to terminal)
;__________________________________________________________________________________________


printLine PROC USES ecx
	mov ecx, dim
	L2:	
		mov eax, 16				;this section sets the text color according to the number in cSquare
		mov ebx, [esi]
		mul ebx					;we know that if the value is zero, no amount of multiplication makes it not zero
		mul liveSquareColor			;this way we can print both 0s and 1s with the same code
		call SetTextColor

		mov al, " "				;prints the space character with a different background color if it is a 1
		call WriteChar		
		
		ADD esi, 4				;incriments 1 DWORD to the right
	loop L2



ret
printLine ENDP


;__________________________________________________________________________________________
; checkSquare
;	Checks what point of the array ESI is referring to, and finds where that entry would be
;	located in a grid, and send the ESI value to the correct check function to discover how 
;	many live cells surround it.
; 
; Receives: ESI - memory address pointing to an "entry" in pbored "array"
;__________________________________________________________________________________________


checkSquare PROC USES ecx
	
	;Finds the number value of the bottom left square (default 10x10 grid, LeftCornerNumber = 91)
	;LeftCornerNumber = BoredSize - dim + 1
	mov edx, BoredSize
	sub edx, dim
	inc edx
	mov LeftCornerNumber, edx

	;sets edx to be remainder (aka ecx mod dim), used to check if square is on left or right side of grid
	;ecx mod dim
	mov eax, ecx
	mov edx, 0
	div dim			;edx = ecx mod dim
	
	;eax = ecx + dim
	mov eax, dim
	add eax, ecx
					;this section is several carefully nested if statements to send it off to the correct set of logic

		.IF ecx <= dim							;top row

			.IF ecx == 1						;top left corner
				call TopLeftCornerCheck

			.ELSEIF ecx == dim
				call TopRightCornerCheck			;top right corner
			
			.ELSE
				call TopCheck
			.ENDIF
			
		.ELSEIF eax > BoredSize					;bottom row

			.IF ecx == BoredSize				;bottom right corner
				call BottomRightCornerCheck

			.ELSEIF ecx == LeftCornerNumber		;bottom left corner
				call BottomLeftCornerCheck

			.ELSE
				call BottomCheck
			.ENDIF
			

		.ELSEIF edx == 1						;left side (excluding top and bottom corners)

			call LeftCheck

		.ELSEIF edx == 0						;right side (excluding top and bottom corners)

			call RightCheck

		.ELSE
			call CenterCheck					;center squares

		.ENDIF


ret
checkSquare ENDP



;__________________________________________________________________________________________
; TopLeftCornerCheck
;	Checks the number of live squares neighboring the top left corner square.
; 
; Receives: ESI - memory address pointing to an "entry" in pbored "array"
; Returns: neighbors - tracks number of live neighbors surrounding a cell (including itself)
;__________________________________________________________________________________________

TopLeftCornerCheck PROC		;this is the only one we will comment, as they are all the same conceptually

	mov neighbors, 0		;resets our neighbor counter

	mov ebx, [esi]			;adds the current cell to the count. if it is alive, the value increases by 1
	add neighbors, ebx
	

	add esi, cRight		;adds the value of the center right cell
	mov ebx, [esi]
	sub esi, cLeft
	add neighbors, ebx


	add esi, bMiddle		;adds the value of the bottom middle cell
	mov ebx, [esi]
	sub esi, bMiddle
	add neighbors, ebx
	

	add esi, bRight		;adds the value of the bottom right cell
	mov ebx, [esi]
	sub esi, bRight
	add neighbors, ebx

ret
TopLeftCornerCheck ENDP

;__________________________________________________________________________________________
; TopCheck
;	Checks the number of live squares neighboring the top row of squares (excluding corners).
; 
; Receives: ESI - memory address pointing to an "entry" in pbored "array"
; Returns: neighbors - tracks number of live neighbors surrounding a cell (including itself)
;
;	See TopLeftCornerCheck for commenting of code in function, almost exactly the same.
;__________________________________________________________________________________________

TopCheck PROC

	mov neighbors, 0

	sub esi, cLeft
	mov ebx, [esi]
	add esi, cRight
	add neighbors, ebx


	mov ebx, [esi]
	add neighbors, ebx
	

	add esi, cRight
	mov ebx, [esi]
	sub esi, cLeft
	add neighbors, ebx


	add esi, bLeft
	mov ebx, [esi]
	sub esi, bLeft
	add neighbors, ebx


	add esi, bMiddle
	mov ebx, [esi]
	sub esi, bMiddle
	add neighbors, ebx
	

	add esi, bRight
	mov ebx, [esi]
	sub esi, bRight
	add neighbors, ebx


ret
TopCheck ENDP


;__________________________________________________________________________________________
; TopRightCornerCheck
;	Checks the number of live squares neighboring the top right corner.
; 
; Receives: ESI - memory address pointing to an "entry" in pbored "array"
; Returns: neighbors - tracks number of live neighbors surrounding a cell (including itself)
;
;	See TopLeftCornerCheck for commenting of code in function, almost exactly the same.
;__________________________________________________________________________________________

TopRightCornerCheck PROC

	mov neighbors, 0

	sub esi, cLeft
	mov ebx, [esi]
	add esi, cRight
	add neighbors, ebx


	mov ebx, [esi]
	add neighbors, ebx
	

	add esi, bLeft
	mov ebx, [esi]
	sub esi, bLeft
	add neighbors, ebx


	add esi, bMiddle
	mov ebx, [esi]
	sub esi, bMiddle
	add neighbors, ebx
	
ret
TopRightCornerCheck ENDP

;__________________________________________________________________________________________
; LeftCheck
;	Checks the number of live squares neighboring the left side of the grid (excluding corners).
; 
; Receives: ESI - memory address pointing to an "entry" in pbored "array"
; Returns: neighbors - tracks number of live neighbors surrounding a cell (including itself)
;
;	See TopLeftCornerCheck for commenting of code in function, almost exactly the same.
;__________________________________________________________________________________________

LeftCheck PROC

	mov neighbors, 0

	sub esi, uMiddle
	mov ebx, [esi]
	add esi, uMiddle
	add neighbors, ebx


	sub esi, uRight
	mov ebx, [esi]
	add esi, uRight
	add neighbors, ebx


	mov ebx, [esi]
	add neighbors, ebx
	

	add esi, cRight
	mov ebx, [esi]
	sub esi, cLeft
	add neighbors, ebx

	add esi, bMiddle
	mov ebx, [esi]
	sub esi, bMiddle
	add neighbors, ebx
	

	add esi, bRight
	mov ebx, [esi]
	sub esi, bRight
	add neighbors, ebx

ret
LeftCheck ENDP


;__________________________________________________________________________________________
; CenterCheck
;	Checks the number of live squares neighboring the center squares.
; 
; Receives: ESI - memory address pointing to an "entry" in pbored "array"
; Returns: neighbors - tracks number of live neighbors surrounding a cell (including itself)
;
;	See TopLeftCornerCheck for commenting of code in function, almost exactly the same.
;__________________________________________________________________________________________

CenterCheck PROC

	mov neighbors, 0

	sub esi, uLeft
	mov ebx, [esi]
	add esi, uLeft
	add neighbors, ebx


	sub esi, uMiddle
	mov ebx, [esi]
	add esi, uMiddle
	add neighbors, ebx


	sub esi, uRight
	mov ebx, [esi]
	add esi, uRight
	add neighbors, ebx


	sub esi, cLeft
	mov ebx, [esi]
	add esi, cRight
	add neighbors, ebx


	mov ebx, [esi]
	add neighbors, ebx
	

	add esi, cRight
	mov ebx, [esi]
	sub esi, cLeft
	add neighbors, ebx


	add esi, bLeft
	mov ebx, [esi]
	sub esi, bLeft
	add neighbors, ebx


	add esi, bMiddle
	mov ebx, [esi]
	sub esi, bMiddle
	add neighbors, ebx
	

	add esi, bRight
	mov ebx, [esi]
	sub esi, bRight
	add neighbors, ebx

ret
CenterCheck ENDP


;__________________________________________________________________________________________
; RightCheck
;	Checks the number of live squares neighboring the right side of the grid (excluding corners).
; 
; Receives: ESI - memory address pointing to an "entry" in pbored "array"
; Returns: neighbors - tracks number of live neighbors surrounding a cell (including itself)
;
;	See TopLeftCornerCheck for commenting of code in function, almost exactly the same.
;__________________________________________________________________________________________

RightCheck PROC

	mov neighbors, 0

	sub esi, uLeft
	mov ebx, [esi]
	add esi, uLeft
	add neighbors, ebx


	sub esi, uMiddle
	mov ebx, [esi]
	add esi, uMiddle
	add neighbors, ebx

	
	sub esi, cLeft
	mov ebx, [esi]
	add esi, cRight
	add neighbors, ebx


	mov ebx, [esi]
	add neighbors, ebx


	add esi, bLeft
	mov ebx, [esi]
	sub esi, bLeft
	add neighbors, ebx


	add esi, bMiddle
	mov ebx, [esi]
	sub esi, bMiddle
	add neighbors, ebx
	
ret
RightCheck ENDP


;__________________________________________________________________________________________
; BottomLeftCornerCheck
;	Checks the number of live squares neighboring the bottom left corner square.
; 
; Receives: ESI - memory address pointing to an "entry" in pbored "array"
; Returns: neighbors - tracks number of live neighbors surrounding a cell (including itself)
;
;	See TopLeftCornerCheck for commenting of code in function, almost exactly the same.
;__________________________________________________________________________________________

BottomLeftCornerCheck PROC

	mov neighbors, 0

	sub esi, uMiddle
	mov ebx, [esi]
	add esi, uMiddle
	add neighbors, ebx


	sub esi, uRight
	mov ebx, [esi]
	add esi, uRight
	add neighbors, ebx

	mov ebx, [esi]
	add neighbors, ebx
	

	add esi, cRight
	mov ebx, [esi]
	sub esi, cLeft
	add neighbors, ebx

ret
BottomLeftCornerCheck ENDP


;__________________________________________________________________________________________
; BottomCheck
;	Checks the number of live squares neighboring the bottom row (excluding corners).
; 
; Receives: ESI - memory address pointing to an "entry" in pbored "array"
; Returns: neighbors - tracks number of live neighbors surrounding a cell (including itself)
;
;	See TopLeftCornerCheck for commenting of code in function, almost exactly the same.
;__________________________________________________________________________________________

BottomCheck PROC

	mov neighbors, 0

	sub esi, uLeft
	mov ebx, [esi]
	add esi, uLeft
	add neighbors, ebx


	sub esi, uMiddle
	mov ebx, [esi]
	add esi, uMiddle
	add neighbors, ebx


	sub esi, uRight
	mov ebx, [esi]
	add esi, uRight
	add neighbors, ebx


	sub esi, cLeft
	mov ebx, [esi]
	add esi, cRight
	add neighbors, ebx


	mov ebx, [esi]
	add neighbors, ebx
	

	add esi, cRight
	mov ebx, [esi]
	sub esi, cLeft
	add neighbors, ebx



ret
BottomCheck ENDP


;__________________________________________________________________________________________
; BottomRightCornerCheck
;	Checks the number of live squares neighboring the bottom right corner square.
; 
; Receives: ESI - memory address pointing to an "entry" in pbored "array"
; Returns: neighbors - tracks number of live neighbors surrounding a cell (including itself)
;
;	See TopLeftCornerCheck for commenting of code in function, almost exactly the same.
;__________________________________________________________________________________________

BottomRightCornerCheck PROC

	mov neighbors, 0

	sub esi, uLeft
	mov ebx, [esi]
	add esi, uLeft
	add neighbors, ebx


	sub esi, uMiddle
	mov ebx, [esi]
	add esi, uMiddle
	add neighbors, ebx


	sub esi, cLeft
	mov ebx, [esi]
	add esi, cRight
	add neighbors, ebx


	mov ebx, [esi]
	add neighbors, ebx	

ret
BottomRightCornerCheck ENDP


;__________________________________________________________________________________________
; TNG
;	Loops through each cell and determines if it should be alive or dead in the next generation,
;	sets fbored with the future generation, then copies fbored into pbored.
; 
; Receives: fbored - memory location of future "array"
; Returns: pbored - set with the next generation
;__________________________________________________________________________________________

TNG PROC uses ecx
mov ecx, BoredSize
mov esi, fbored					;because we count backwards, we go to fbored and subtract 4, gets us to the end of pbored
sub esi, 4
mov eax, 0	
	L22:							;this loop goes through the whole array, backwords because the loop counter counts down

		call CheckSquare			;goes and finds the number of alive cells and leaves it in neighbors

		call its_quantumly_alive		;uses the value in neighbors to determine the what the next generation should be, and sets it there
		

		sub esi, 4
	LOOP L22

	call f_to_p

ret
TNG ENDP

;__________________________________________________________________________________________
; its_quantumly_alive
;	Uses neighbors variable value to determine if the cell in question should live or die
;	in the next generation.
; 
; Receives: neighbors - holds number of live neighbors for cell in question (including self)
; Returns: fbored - sets corresponding fbored cell to alive or dead, based on rules of game
;__________________________________________________________________________________________

its_quantumly_alive PROC uses esi

mov eax, neighbors
.IF eax == 3							;checks if it will be alive, then sets the value in the future board
	add esi, bored_size_bytes
	mov ebx, 1
	mov [esi], ebx
	sub esi, bored_size_bytes
.ELSEIF eax == 4						;if it is 4, then it needs to check if the cell itself is alive or not. If it is alive, then it will be alive, otherwise dead
	mov ebx, [esi]

	.IF ebx == 1
		add esi, bored_size_bytes
		mov [esi], ebx
		sub esi, bored_size_bytes

	.ELSE
		add esi, bored_size_bytes
		mov ebx, 0					;need to change back to 0
		mov [esi], ebx
		sub esi, bored_size_bytes
	.ENDIF
.ELSE
	add esi, bored_size_bytes			;all other numbers die, either to overpopulation or underpopulation
	mov ebx, 0						;can change to zero
	mov [esi], ebx
	sub esi, bored_size_bytes
.ENDIF

ret
its_quantumly_alive ENDP



;__________________________________________________________________________________________
; f_to_p
;	Copies fbored into pbored.
; 
; Receives: fbored - memory location of future "array"
; Returns: pbored - now identical to contents of fbored
;__________________________________________________________________________________________


f_to_p PROC uses ecx
mov esi, fbored
mov ecx, BoredSize

L66:								;pretty simple, copies the future to the present cell by cell
	mov eax, [esi]
	sub esi, bored_size_bytes
	mov [esi], eax
	add esi, bored_size_bytes
	add esi, 4
loop L66

ret
f_to_p ENDP



;__________________________________________________________________________________________
; game_loop
;	Loop runs 25 generations of the game, delaying (so the user can see each generation),
;	calling the logic on the cells, then displaying the next generation.
; 
; Returns: (Prints to terminal)
;__________________________________________________________________________________________

game_loop PROC
	mov ecx, 999			;this sets how many generations are made before it quits the program
	L16:

		mov eax,300
		call Delay		;gives time for the user to observe before going on

		call TNG			;does the math for the next generation(haha, aren't we funny)


		call displayBoard	;prints out the new board

	loop L16

ret
game_loop ENDP


;__________________________________________________________________________________________
; glider
;	Places a glider pattern in the middle of pbored.
; 
; Receives: pbored - memory location of present array
;		  uMiddle, cRight, bLeft, bMiddle, bRight - variables to locate where in regards to the center square the pattern will go
; Returns: pbored - all 0s with 1s in the glider pattern positions
;__________________________________________________________________________________________

glider PROC 

mov edx, 0					;this section sets our indexor, esi, to the center of the board(only works for odd numbers, since that would be the center)
mov esi, pbored
mov eax, bored_size_bytes
mov ebx, 2
div ebx
sub eax, ebx
add esi, eax


;sets edx to be remainder (aka dim mod 2), used to check if dim is even or odd
;dim mod 2
mov eax, dim
mov edx, 0
mov ebx, 2
div ebx			;edx = dim mod 2

;this checks if it is odd, and if so lets it through. if even, it adds half of the line size and subtracts 2 bytes, which puts us at the center of the board for even numbers
.IF edx == 0
	mov eax, dim
	mov ebx, 2
	mul ebx
	sub eax, 2
	add esi, eax
.ENDIF



mov ebx, 1			;sets it to 1 so we are putting in live cells

sub esi, uMiddle		;the rest of this is just painting the correct cells to be alive
mov [esi], ebx
add esi, uMiddle

add esi, cRight
mov [esi], ebx
sub esi, cRight

add esi, bLeft
mov [esi], ebx
sub esi, bLeft

add esi, bMiddle
mov [esi], ebx
sub esi, bMiddle

add esi, bRight
mov [esi], ebx
sub esi, bRight

ret
glider ENDP



;__________________________________________________________________________________________
; flasher
;	Places a flasher pattern in the middle of pbored.
; 
; Receives: pbored - memory location of present array
;		  cLeft, bMiddle, cRight - variables to locate where in regards to the center square the pattern will go
; Returns: pbored - all 0s with 1s in the flasher pattern positions
;__________________________________________________________________________________________

flasher PROC ;this whole thing is the same as glider, just look there for comments

mov edx, 0
mov esi, pbored
mov eax, bored_size_bytes
mov ebx, 2
div ebx
sub eax, ebx
add esi, eax

;sets edx to be remainder (aka dim mod 2), used to check if dim is even or odd
;dim mod 2
mov eax, dim
mov edx, 0
mov ebx, 2
div ebx			;edx = dim mod 2

.IF edx == 0
	mov eax, dim
	mov ebx, 2
	mul ebx
	sub eax, 2
	add esi, eax
.ENDIF



;SET ESI TO THE MIDDLE OF THE BOARD HERE

mov ebx, 1

sub esi, cLeft
mov [esi], ebx
add esi, cLeft

sub esi, uMiddle
mov [esi], ebx
add esi, uMiddle

mov [esi], ebx

add esi, cRight
mov [esi], ebx
sub esi, cRight



ret
flasher ENDP

;__________________________________________________________________________________________
; bomb
;	Places a bomb pattern in the middle of pbored.
; 
; Receives: pbored - memory location of present array
;		  uMiddle, uRight, cLeft, bMiddle, bRight - variables to locate where in regards to the center square the pattern will go
; Returns: pbored - all 0s with 1s in the bomb pattern positions
;__________________________________________________________________________________________

bomb PROC		;again, same as glider, just look there for comments

mov edx, 0
mov esi, pbored
mov eax, bored_size_bytes
mov ebx, 2
div ebx
sub eax, ebx
add esi, eax

;sets edx to be remainder (aka dim mod 2), used to check if dim is even or odd
;dim mod 2
mov eax, dim
mov edx, 0
mov ebx, 2
div ebx			;edx = dim mod 2

.IF edx == 0
	mov eax, dim
	mov ebx, 2
	mul ebx
	sub eax, 2
	add esi, eax
.ENDIF



;SET ESI TO THE MIDDLE OF THE BOARD HERE

mov ebx, 1

sub esi, uMiddle
mov [esi], ebx
add esi, uMiddle

sub esi, uRight
mov [esi], ebx
add esi, uRight

sub esi, cLeft
mov [esi], ebx
add esi, cLeft

mov [esi], ebx

add esi, bMiddle
mov [esi], ebx
sub esi, bMiddle

add esi, bRight
mov [esi], ebx
sub esi, bRight

add esi, 8
mov [esi], ebx
sub esi, 8

ret
bomb ENDP

;__________________________________________________________________________________________
; patterns
;	Grabs user input to determine which pattern to fill pbored with.
; 
; Receives: user input using ReadDec
; Returns: pbored - updated with user chosen pattern
;__________________________________________________________________________________________

patterns PROC
	mov edx, OFFSET prompt4
	call WriteString
	
	call ReadDec
						;this reads in the input from the user, then calls the correct pattern
	.IF eax == 1
		call glider
	.ELSEIF eax == 2
		call flasher
	.ELSEIF eax == 3
		call bomb
	.ENDIF

	call Clrscr

ret
patterns ENDP


END main