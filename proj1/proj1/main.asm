;
; proj1.asm
;
; Created: 11/14/2019 12:36:54 PM
; Author: Shaoyang Zhang
; Function: This program simulates a window shade control system, 
; where having 4 Windows and each window has 4 opaque levels, i.e. 0-3.
; there are 4 control mode: Initial(S), Local(L), Central(C) and Emergency(!!)
; The program takes input as control from keypad and displays the window status
; on the LED bar and display.
;

; Port C -> LCD data (D0-D7) to PORTC0-7
; Port A -> LCD control
; PE3(PE5) -> LED0-1
; PE4(PE2) -> LED2-3
; PE5(PE3) -> LED4-5
; PH5(PH8) -> LED6-7 
; Port L -> Keypad, high 4 bits for column selection, low four bits for reading rows.
; PL7-4 to R0-3, PL3-0 to C0-3.
; Port D -> Button push (PB0-1) as well as INT0-1

; Replace with your application code
.include "m2560def.inc"

.def row   = r16				; current row number
.def col   = r17				; current column number
.def rmask = r18				; mask for current row during scan
.def cmask = r19				; mask for current column during scan
.def temp1 = r20  
.def temp2 = r21
.def lcd_temp = r22				; register to store the lcd data / command
.def win_busy = r23				; r25:r24 is used to do the counter increment

.equ PORTLDIR =0xF0			; use PortL for input/output from keypad: PL7-4, output, PL3-0, input
.equ INITCOLMASK = 0xEF		; scan from the leftmost column C0, the value to mask output
.equ INITROWMASK = 0x01		; scan from the bottom row R0
.equ ROWMASK  =0x0F			; low four bits are output from the keypad. This value mask the high 4 bits.

.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4

.equ WIN_NUM = 4

.equ DIM_LVL0 = 0
.equ DIM_LVL1 = 16
.equ DIM_LVL2 = 80
.equ DIM_LVL3 = 255

.equ W1_ADDR = 0xC4
.equ W2_ADDR = 0xC7
.equ W3_ADDR = 0xCA
.equ W4_ADDR = 0xCD
.equ MODE_ADDR = 0x81

.equ TIMER_1S = 500			; Timer1,3,4,5 under compare mode, OVF = 256*2*64/16,000,000 = 2048us thus num = 1s / 2048us = 500

; Define variables
.dseg
.org 0x200
Windows:
	.byte WIN_NUM
Mode:
	.byte 2
Counter1:
	.byte 2
Counter2:
	.byte 2
Counter3:
	.byte 2
Counter4:
	.byte 2

; Initial program memory
.cseg
.org 0x0000
	jmp RESET

.org INT0addr
	jmp EXT_INT0
.org INT1addr
	jmp EXT_INT1

.org OVF1addr	; Jump to the interrupt handler for Timer1 overflow
	jmp Timer1OVF
.org OVF3addr
	jmp Timer3OVF
.org OVF4addr
	jmp Timer4OVF
.org OVF5addr
	jmp Timer5OVF

; Initial constant value
WINS_INI:
	.db 0, 0, 0, 0	;take 4 bytes
MODE_INI:
	.db "S:"

; Define macros
.macro do_lcd_command
	ldi lcd_temp, @0
	rcall lcd_command
	rcall lcd_wait
.endmacro

.macro do_lcd_command_reg
	mov lcd_temp, @0
	rcall lcd_command
	rcall lcd_wait
.endmacro

.macro do_lcd_data
	ldi lcd_temp, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro do_lcd_data_reg
	mov lcd_temp, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro lcd_set
	sbi PORTA, @0
.endmacro

.macro lcd_clr
	cbi PORTA, @0
.endmacro

.macro do_led_dimming	; @0 = opaque levels @1 = {OCR3AL,OCR3BL,OCR3CL,OCR4CL} @2 = {OCR3AH,OCR3BH,OCR3CH,OCR4CH}
	cpi @0, 0
	breq clear0
	cpi @0, 1
	breq lvl1
	cpi @0, 2
	breq lvl2

dark:
	ldi temp2, DIM_LVL3
	sts @1, temp2
	jmp dim_end

clear0:
	ldi temp2, DIM_LVL0
	sts @1, temp2
	jmp dim_end

lvl1:
	ldi temp2, DIM_LVL1
	sts @1, temp2
	jmp dim_end

lvl2:
	ldi temp2, DIM_LVL2
	sts @1, temp2

dim_end:
	clr temp2
	sts @2, temp2
.endmacro

.macro wins_local_ctrl						; @0 = window no. i.e. 0-3
	sbr win_busy, 1<<@0
	
	ldi yh, high(Windows)					; pointer to current windows status
	ldi yl, low(Windows)					; in the data memory

	ldd temp1, y+@0
	cpi col, 0
	breq wins_up
wins_down:
	mov temp2, temp1
	cpi temp1, 0
	breq wins_end
	dec temp1
	std y+@0, temp1
	jmp wins_end
wins_up:
	mov temp2, temp1
	cpi temp1, 3
	breq wins_end
	inc temp1
	std y+@0, temp1
wins_end:
	nop
.endmacro

.macro centrl_ctrl
	ldi yh, high(Windows)		; pointer to current windows status
	ldi yl, low(Windows)		; in the data memory
	ld temp1, y
	cpi temp1, @0
	breq clr_busy1
ctrl1:
	ldi temp1, @0
	st y, temp1
	ldi temp1, 1<<TOIE1			; Timer1 enable
	sts TIMSK1, temp1			; T/C1 interrupt enable
	jmp ctrl2
clr_busy1:
	cbr win_busy, 1<<0
ctrl2:
	ldd temp1, y+1
	cpi temp1, @0
	breq clr_busy2
	ldi temp1, @0
	std y+1, temp1
	ldi temp1, 1<<TOIE3			; Timer3 enable
	sts TIMSK3, temp1			; T/C3 interrupt enable
	jmp ctrl3
clr_busy2:
	cbr win_busy, 1<<1
ctrl3:
	ldd temp1, y+2
	cpi temp1, @0
	breq clr_busy3
	ldi temp1, @0
	std y+2, temp1
	ldi temp1, 1<<TOIE4			; Timer4 enable
	sts TIMSK4, temp1			; T/C4 interrupt enable
	jmp ctrl4
clr_busy3:
	cbr win_busy, 1<<2
ctrl4:
	ldd temp1, y+3
	cpi temp1, @0
	breq clr_busy4
	ldi temp1, @0
	std y+3, temp1
	ldi temp1, 1<<TOIE5			; Timer5 enable
	sts TIMSK5, temp1			; T/C5 interrupt enable
	jmp ctrl_done
clr_busy4:
	cbr win_busy, 1<<3
ctrl_done:
	nop
.endmacro

; The macro clears a word (2 bytes) in a memory
; the parameter @0 is the memory address for that word
.macro Clear
	ldi YL, low(@0)	; load the memory address to Y
	ldi YH, high(@0)
	clr temp1
	st Y+, temp1	; clear the two bytes at @0 in SRAM
	st Y, temp1
.endmacro


EXT_INT0:
	push lcd_temp				; Save conflict register
	push temp2
	push temp1
	push YH
	push YL
	push r25
	push r24					
	in	 temp1, SREG			; Save SREG
	push temp1

emer_waiting_loop1:
	cpi win_busy, 0
	breq emer_allgood1
	;rcall sleep_5ms
	rjmp emer_waiting_loop1
emer_allgood1:
	sbr win_busy, 0x0F
	;centrl_ctrl 0				; Emergency clear

	;store control mode "!!"
	ldi temp1, '!'
	std y+4, temp1
	ldi temp1, '!'
	std y+5, temp1
	;set mode address
	do_lcd_command MODE_ADDR
	do_lcd_data '!'
	do_lcd_data '!'

	rcall sleep_halfsec

	ldi yh, high(Windows)		; pointer to current windows status
	ldi yl, low(Windows)		; in the data memory

	clr temp1
	std y+0, temp1
	std y+1, temp1
	std y+2, temp1
	std y+3, temp1

	do_led_dimming temp1, OCR3AL, OCR3AH
	do_led_dimming temp1, OCR3BL, OCR3BH
	do_led_dimming temp1, OCR3CL, OCR3CH
	do_led_dimming temp1, OCR4CL, OCR4CH

	do_lcd_command W1_ADDR
	do_lcd_data '0'
	do_lcd_command W2_ADDR
	do_lcd_data '0'
	do_lcd_command W3_ADDR
	do_lcd_data '0'
	do_lcd_command W4_ADDR
	do_lcd_data '0'

	clr win_busy

/*
emer_ending_loop1:
	cpi win_busy, 0
	breq emer_end1
	;rcall sleep_5ms
	rjmp emer_ending_loop1
*/

emer_end1:
	pop temp1                       ; Restore SREG
	out SREG,temp1                  ; Store data from temp1 to SREG
	pop r24
	pop r25
	pop YL
	pop YH
	pop temp1                       ; Restore register
	pop temp2
	pop lcd_temp
	reti                            ; Return from the interrupt

EXT_INT1:
	push lcd_temp				; Save conflict register
	push temp2
	push temp1
	push YH
	push YL
	push r25
	push r24					
	in	 temp1, SREG			; Save SREG
	push temp1

emer_waiting_loop2:
	cpi win_busy, 0
	breq emer_allgood2
	;rcall sleep_5ms
	rjmp emer_waiting_loop2
emer_allgood2:
	sbr win_busy, 0x0F

	;store control mode "!!"
	ldi temp1, '!'
	std y+4, temp1
	ldi temp1, '!'
	std y+5, temp1
	;set mode address
	do_lcd_command MODE_ADDR
	do_lcd_data '!'
	do_lcd_data '!'

	;all windows to be clear
	rcall sleep_halfsec

	ldi yh, high(Windows)		; pointer to current windows status
	ldi yl, low(Windows)		; in the data memory

	clr temp1
	std y+0, temp1
	std y+1, temp1
	std y+2, temp1
	std y+3, temp1

	do_led_dimming temp1, OCR3AL, OCR3AH
	do_led_dimming temp1, OCR3BL, OCR3BH
	do_led_dimming temp1, OCR3CL, OCR3CH
	do_led_dimming temp1, OCR4CL, OCR4CH

	do_lcd_command W1_ADDR
	do_lcd_data '0'
	do_lcd_command W2_ADDR
	do_lcd_data '0'
	do_lcd_command W3_ADDR
	do_lcd_data '0'
	do_lcd_command W4_ADDR
	do_lcd_data '0'

	clr win_busy

emer_end2:
	pop temp1                       ; Restore SREG
	out SREG,temp1                  ; Store data from temp1 to SREG
	pop r24
	pop r25
	pop YL
	pop YH
	pop temp1                       ; Restore register
	pop temp2
	pop lcd_temp
	reti                            ; Return from the interrupt


Timer1OVF:					; interrupt subroutine for Timer1
	push lcd_temp
	push temp2
	push temp1
	push YH					; Save all conflict registers in the prologue.
	push YL
	push r25
	push r24
	in	 temp1, SREG		; Save SREG
	push temp1

	ldi YL, low(Counter1)	; Load the address of the temporary
	ldi YH, high(Counter1)	; counter.
	ld r24, Y+
	ld r25, Y
	adiw r25:r24, 1			; Increase the temporary counter by one.

	cpi r24, low(TIMER_1S/2)
	brne NotSecond1
	cpi r25, high(TIMER_1S/2)
	brne NotSecond1

	lds temp1, TIMSK1
	cbr temp1, 1<<TOIE1
	sts TIMSK1, temp1		; T/C1 interrupt disable

	ldi YL, low(Windows)
	ldi YH, high(Windows)
	ldd temp1, Y+0			; Read W1 opaque level
	do_led_dimming temp1, OCR3AL, OCR3AH
	;set mode address
	do_lcd_command W1_ADDR
	subi temp1, -'0'
	do_lcd_data_reg temp1

	cbr win_busy, 1<<0		; Clear win1 busy flag
	Clear Counter1

	rjmp EndIF1

NotSecond1:
	st Y, r25				; Store the value of the temporary counter.
	st -Y, r24

EndIF1:
	pop temp1               ; Restore SREG
	out SREG,temp1          ; Store data from temp1 to SREG
	pop r24					; Epilogue starts;
	pop r25					; Restore all conflict registers from the stack.
	pop YL
	pop YH
	pop temp1
	pop temp2
	pop lcd_temp
	reti					; Return from the interrupt.


Timer3OVF:
	push lcd_temp
	push temp2
	push temp1
	push YH					; Save all conflict registers in the prologue.
	push YL
	push r25
	push r24
	in	 temp1, SREG		; Save SREG
	push temp1

	ldi YL, low(Counter2)	; Load the address of the temporary
	ldi YH, high(Counter2)	; counter.
	ld r24, Y+
	ld r25, Y
	adiw r25:r24, 1			; Increase the temporary counter by one.

	cpi r24, low(TIMER_1S/2)
	brne NotSecond2
	cpi r25, high(TIMER_1S/2)
	brne NotSecond2

	lds temp1, TIMSK3
	cbr temp1, 1<<TOIE3
	sts TIMSK3, temp1		; T/C3 interrupt disable
	
	ldi YL, low(Windows)
	ldi YH, high(Windows)
	ldd temp1, Y+1			; Read W2 opaque level
	
	;do_lcd_command 0xC0
	;do_lcd_data 'D'
	
	do_led_dimming temp1, OCR3BL, OCR3BH
	;set mode address
	do_lcd_command W2_ADDR
	subi temp1, -'0'
	do_lcd_data_reg temp1

	cbr win_busy, 1<<1
	Clear Counter2

	rjmp EndIF2

NotSecond2:
	st Y, r25				; Store the value of the temporary counter.
	st -Y, r24
	
	;do_lcd_command 0xC0
	;subi r24, -'0'
	;do_lcd_data_reg r24

EndIF2:
	pop temp1               ; Restore SREG
	out SREG,temp1          ; Store data from temp1 to SREG
	pop r24					; Epilogue starts;
	pop r25					; Restore all conflict registers from the stack.
	pop YL
	pop YH
	pop temp1
	pop temp2
	pop lcd_temp
	reti					; Return from the interrupt.


Timer4OVF:
	push lcd_temp
	push temp2
	push temp1
	push YH					; Save all conflict registers in the prologue.
	push YL
	push r25
	push r24
	in	 temp1, SREG		; Save SREG
	push temp1

	ldi YL, low(Counter3)	; Load the address of the temporary
	ldi YH, high(Counter3)	; counter.
	ld r24, Y+
	ld r25, Y
	adiw r25:r24, 1			; Increase the temporary counter by one.

	cpi r24, low(TIMER_1S/2)
	brne NotSecond3
	cpi r25, high(TIMER_1S/2)
	brne NotSecond3

	lds temp1, TIMSK4
	cbr temp1, 1<<TOIE4
	sts TIMSK4, temp1		; T/C4 interrupt disable

	ldi YL, low(Windows)
	ldi YH, high(Windows)
	ldd temp1, Y+2			; Read W3 opaque level

	;do_lcd_command 0xC0
	;do_lcd_data 'D'

	do_led_dimming temp1, OCR3CL, OCR3CH
	;set mode address
	do_lcd_command W3_ADDR
	subi temp1, -'0'
	do_lcd_data_reg temp1

	cbr win_busy, 1<<2
	Clear Counter3

	rjmp EndIF3

NotSecond3:
	st Y, r25				; Store the value of the temporary counter.
	st -Y, r24

EndIF3:
	pop temp1               ; Restore SREG
	out SREG,temp1          ; Store data from temp1 to SREG
	pop r24					; Epilogue starts;
	pop r25					; Restore all conflict registers from the stack.
	pop YL
	pop YH
	pop temp1
	pop temp2
	pop lcd_temp
	reti					; Return from the interrupt.


Timer5OVF:
	push lcd_temp
	push temp2
	push temp1
	push YH					; Save all conflict registers in the prologue.
	push YL
	push r25
	push r24
	in	 temp1, SREG		; Save SREG
	push temp1

	ldi YL, low(Counter4)	; Load the address of the temporary
	ldi YH, high(Counter4)	; counter.
	ld r24, Y+
	ld r25, Y
	adiw r25:r24, 1		; Increase the temporary counter by one.

	cpi r24, low(TIMER_1S/2)
	brne NotSecond4
	cpi r25, high(TIMER_1S/2)
	brne NotSecond4

	lds temp1, TIMSK5
	cbr temp1, 1<<TOIE5
	sts TIMSK5, temp1		; T/C5 interrupt disable

	ldi YL, low(Windows)
	ldi YH, high(Windows)
	ldd temp1, Y+3			; Read W4 opaque level
	do_led_dimming temp1, OCR4CL, OCR4CH
	;set mode address
	do_lcd_command W4_ADDR
	subi temp1, -'0'
	do_lcd_data_reg temp1

	cbr win_busy, 1<<3		; Clear win4 busy flag
	Clear Counter4

	rjmp EndIF4

NotSecond4:
	st Y, r25				; Store the value of the temporary counter.
	st -Y, r24

EndIF4:
	pop temp1               ; Restore SREG
	out SREG,temp1          ; Store data from temp1 to SREG
	pop r24					; Epilogue starts;
	pop r25					; Restore all conflict registers from the stack.
	pop YL
	pop YH
	pop temp1
	pop temp2
	pop lcd_temp
	reti					; Return from the interrupt.


RESET:
	cli											; Disable interrupts

	;setup INT0 and INT1
	ldi temp1, (2 << ISC00) | (2 << ISC10)      ; Set INT0 and INT1 as falling edge triggered interrupt
	sts EICRA, temp1							; Store the value of temp1 to EICRA

	in temp1, EIMSK								; Enable INT0 and INT1
	ori temp1, (1 << INT0) | (1 << INT1)        ; Logical OR register with immediate
	out EIMSK, temp1							; Store data from temp1 to EIMSK
	;sbi EIMSK, (1 << INT0) | (1 << INT1)

	;setup Timer1,3,4,5
	;1,5 on phase correct PWM mode but no connection on OC1x and OC5x
	;3,4 on phase correct PWM mode
	;ldi temp1, 0x00							; Set timer1,5 on normal mode

	ldi temp1, (1<<WGM10)						; set timer1,5 on Phase Correct PWM mode
	sts TCCR1A, temp1
	;ldi temp1, (1<<WGM00)
	;sts TCCR0A, temp1
	;sts TCCR2A, temp1
	ldi temp1, (1<<WGM50)
	sts TCCR5A, temp1
	
	ldi temp1, 0x03								; Prescaling value=64, counting 1024 us
	sts TCCR1B, temp1
	;sts TCCR0B, temp1
	;sts TCCR2B, temp1
	sts TCCR5B, temp1

	;set Timer3 to Phase Correct PWM on OC3A, OC3B, OC3C
	;clear when up-counting match, set when down-counting match
	ldi temp1, (1<<WGM30)|(1<<COM3A1)|(1<<COM3B1)|(1<<COM3C1)
	sts TCCR3A, temp1	
	ldi temp1, (1<<CS30)|(1<<CS31)
	sts TCCR3B, temp1

	;set Timer4 to Phase Correct PWM on OC4C
	ldi temp1, (1<<WGM40)|(1<<COM4C1)
	sts TCCR4A, temp1
	ldi temp1, (1<<CS40)|(1<<CS41)
	sts TCCR4B, temp1

	sei											; Enable global interrupt

	;setup keypad
	ldi temp1, PORTLDIR			; columns are outputs, rows are inputs
	sts	DDRL, temp1

	;setup LEDs
	ldi temp1, 0b00111000
	out DDRE, temp1				; Bits 3,4,5 will function as OC3A,B,C.

	ldi temp1, 0b00100000
	sts DDRH, temp1				; Bit 5 will function as OC4C.

	;setup LCD
	ldi temp1, low(RAMEND)
	out SPL, temp1
	ldi temp1, high(RAMEND)
	out SPH, temp1

	ser temp1
	out DDRC, temp1 ;set PortC as output
	out DDRA, temp1 ;set PortA as output
	clr temp1
	out PORTC, temp1
	out PORTA, temp1

	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_5ms
	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_1ms
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00001000 ; display off
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001100 ; display on, cursor off, w/o blink

	;initialization
	;clear window busy flag and timer counter
	clr win_busy
	Clear Counter1
	Clear Counter2
	Clear Counter3
	Clear Counter4

	;load data from program memory to data memory
	ldi zh, high(WINS_INI<<1)				; pointer to the initial status of windows
	ldi zl, low(WINS_INI<<1)				; in the program memory

	ldi yh, high(Windows)					; pointer to current windows status
	ldi yl, low(Windows)					; in the data memory

	clr temp1
load:
	lpm temp2, z+
	st y+, temp2
	inc temp1
	cpi temp1, WIN_NUM+2
	brlt load
	
	;LEDs and Display initialization
	;set the value to control PWM duty cycle, i.e. LEDs dimming
	ldi yh, high(Windows)
	ldi yl, low(Windows)
	ld temp1, y
	do_led_dimming temp1, OCR3AL, OCR3AH
	ldd temp1, y+1
	do_led_dimming temp1, OCR3BL, OCR3BH
	ldd temp1, y+2
	do_led_dimming temp1, OCR3CL, OCR3CH
	ldd temp1, y+3
	do_led_dimming temp1, OCR4CL, OCR4CH

	;initialize Display
	;1st line
	do_lcd_data 0x20
	;lds temp1, Mode
	ldd temp1, y+4
	do_lcd_data_reg temp1
	;do_lcd_data ':'
	ldd temp1, y+5
	do_lcd_data_reg temp1

	;do_lcd_data 0x20
	do_lcd_data 0x20
	do_lcd_data 'W'
	do_lcd_data '1'

	do_lcd_data 0x20
	do_lcd_data 'W'
	do_lcd_data '2'
	
	do_lcd_data 0x20
	do_lcd_data 'W'
	do_lcd_data '3'
	
	do_lcd_data 0x20
	do_lcd_data 'W'
	do_lcd_data '4'

	;2nd line
	do_lcd_command 0xC0			; set second line address(0x40)

	do_lcd_data 0x20
	do_lcd_data 0x20
	do_lcd_data 0x20
	do_lcd_data 0x20

	ld temp1, y
	subi temp1, -'0'
	do_lcd_data_reg temp1		; W1 = 0x44
	
	do_lcd_data 0x20
	do_lcd_data 0x20

	ldd temp1, y+1				; W2 = 0x47
	subi temp1, -'0'
	do_lcd_data_reg temp1

	do_lcd_data 0x20
	do_lcd_data 0x20

	ldd temp1, y+2				; W3 = 0x4A
	subi temp1, -'0'
	do_lcd_data_reg temp1

	do_lcd_data 0x20
	do_lcd_data 0x20

	ldd temp1, y+3				; W4 = 0x4D
	subi temp1, -'0'
	do_lcd_data_reg temp1

;halt:
;	rjmp halt

main:
	ldi cmask, INITCOLMASK		; initial column mask
	clr	col						; initial column	

colloop:
	cpi col, 4
	breq main
	sts	PORTL, cmask			; set column to mask value (one column off)
	ldi temp1, 0xFF

delay:
	dec temp1
	brne delay

	lds	temp1, PINL				; read PORTL
	andi temp1, ROWMASK
	cpi temp1, 0x0F				; check if any rows are on
	breq nextcol
								; if yes, find which row is on
	ldi rmask, INITROWMASK		; initialise row check
	clr	row						; initial row

rowloop:
	cpi row, 4
	breq nextcol
	mov temp2, temp1
	and temp2, rmask			; check masked bit
	breq convert 				; if bit is clear, convert the bitcode
	inc row						; else move to the next row
	lsl rmask					; shift the mask to the next bit
	jmp rowloop

nextcol:
	lsl cmask					; else get new mask by shifting and 
	inc col						; increment column value
	jmp colloop					; and check the next column

convert:
	cpi row, 0					; if row is 3 window1 or central control
	breq win4
	cpi row, 1
	breq win3
	cpi row, 2
	breq jump2win2
	jmp win1c

jump2win2:
	jmp win2

win4:
	cpi col, 2
	brlo win4_ctrl
	jmp main
win4_ctrl:
	sbrc win_busy, 3
	jmp main					; if win4 is busy, rescan the keypad

	wins_local_ctrl 3

	cp temp1, temp2
	brne start_timer5
	cbr win_busy, 1<<3
	jmp local_end
start_timer5:
	ldi temp1, 1<<TOIE5			; Timer5 enable
	sts TIMSK5, temp1			; T/C5 interrupt enable	
	jmp local_end

win3:
	cpi col, 2
	brlo win3_ctrl
	jmp main
win3_ctrl:
	sbrc win_busy, 2
	jmp main

	wins_local_ctrl 2

	cp temp1, temp2
	brne start_timer4
	cbr win_busy, 1<<2
	jmp local_end
start_timer4:
	ldi temp1, 1<<TOIE4			; Timer4 enable
	sts TIMSK4, temp1			; T/C4 interrupt enable	
	jmp local_end

win2:
	cpi col, 2
	brlo win2_ctrl
	jmp main
win2_ctrl:
	sbrc win_busy, 1
	jmp main

	wins_local_ctrl 1

	cp temp1, temp2
	brne start_timer3
	cbr win_busy, 1<<1
	jmp local_end
start_timer3:
	ldi temp1, 1<<TOIE3			; Timer3 enable
	sts TIMSK3, temp1			; T/C3 interrupt enable	
	jmp local_end

win1c:
	cpi col, 2
	brsh centrl_waiting_loop
win1_ctrl:
	sbrc win_busy, 0
	jmp main

	wins_local_ctrl 0

	cp temp1, temp2
	brne start_timer1
	cbr win_busy, 1<<0
	jmp local_end
start_timer1:
	ldi temp1, 1<<TOIE1			; Timer1 enable
	sts TIMSK1, temp1			; T/C1 interrupt enable

local_end:
	;store control mode "L:"
	ldi temp1, 'L'
	std y+4, temp1
	ldi temp1, ':'
	std y+5, temp1
	;set mode address
	do_lcd_command MODE_ADDR
	do_lcd_data 'L'
	do_lcd_data ':'
	jmp main

centrl_waiting_loop:
	cpi win_busy, 0
	breq centrl_allgood
	;rcall sleep_5ms
	rjmp centrl_waiting_loop
centrl_allgood:
	sbr win_busy, 0x0F
	cpi col, 2
	breq centrl_up
centrl_down:
	centrl_ctrl 0
	jmp centrl_end
centrl_up:
	centrl_ctrl 3
centrl_end:
	;store control mode "C:"
	ldi temp1, 'C'
	std y+4, temp1
	ldi temp1, ':'
	std y+5, temp1
	;set mode address
	do_lcd_command MODE_ADDR
	do_lcd_data 'C'
	do_lcd_data ':'

centrl_ending_loop:
	cpi win_busy, 0
	breq jump2main
	;rcall sleep_5ms
	rjmp centrl_ending_loop

jump2main:
	jmp main


;
; Send a command to the LCD (lcd_temp = r22)
;

lcd_command:
	out PORTC, lcd_temp
	nop
	lcd_set LCD_E
	nop
	nop
	nop
	lcd_clr LCD_E
	nop
	nop
	nop
	ret

lcd_data:
	out PORTC, lcd_temp
	lcd_set LCD_RS
	nop
	nop
	nop
	lcd_set LCD_E
	nop
	nop
	nop
	lcd_clr LCD_E
	nop
	nop
	nop
	lcd_clr LCD_RS
	ret

lcd_wait:
	push lcd_temp
	clr lcd_temp
	out DDRC, lcd_temp
	out PORTC, lcd_temp
	lcd_set LCD_RW
lcd_wait_loop:
	nop
	lcd_set LCD_E
	nop
	nop
    nop
	in lcd_temp, PINC
	lcd_clr LCD_E
	sbrc lcd_temp, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser lcd_temp
	out DDRC, lcd_temp
	pop lcd_temp
	ret

.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4
; 4 cycles per iteration - setup/call-return overhead

sleep_1ms:
	push temp1
	in temp1, SREG
	push temp1
	push r24
	push r25

	ldi r25, high(DELAY_1MS)
	ldi r24, low(DELAY_1MS)
delayloop_1ms:
	sbiw r25:r24, 1
	brne delayloop_1ms

	pop r25
	pop r24
	pop	temp1
	out SREG, temp1
	pop temp1
	ret

sleep_5ms:
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	ret

sleep_halfsec:
	push temp1
	in temp1, SREG
	push temp1
	ldi temp1, 100
delayloop_halfsec:
	rcall sleep_5ms
	dec temp1
	brne delayloop_halfsec
	pop	temp1
	out SREG, temp1
	pop temp1
	ret
	
