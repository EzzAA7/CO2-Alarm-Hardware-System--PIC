;Izz AbuAsab
;Realtime HW

list P = 16F877A
include P16F877A.inc


STATUS EQU 0x03
TRISC	EQU	087		; Data direction
PORTC	EQU	007		; 7-Segment display

TRISA	EQU	085		; Data direction
PORTA	EQU	005		; 7-Segment display
Count	equ	30
TMR0	EQU	01	; Hardware Timer Register
INTCON	EQU	0B	; Interrupt Control Register
OPTREG	EQU	81	; Option Register

ADbin	EQU	31	; Binary input value
;huns	EQU	32	; Hundreds digit in decimal value
;tens	EQU	33	; Tens digit in decimal value
;ones	EQU	34	; Ones digit in decimal value
val1 EQU 35
val2 equ 36

saveW EQU 0x7E        ; variable used for context saving 
saveTemp EQU 0x7F        ; variable used for context saving


org 0x00 ;line 1
goto INIT ;line 2 ($0000)
;org 0x05

; Start Program ********************************************
INIT
	BANKSEL PORTC
	MOVLW 0x00 ;outputs
	MOVWF PORTC ; and set bit direction

	; initialize ADCON1
	BANKSEL	TRISC		; Select bank 1
	MOVLW	B'00000011'	; Analogue input setup code
	MOVWF	ADCON1		; Left justify result, ; Port A = analogue inputs
	
	; initialize ADCON1
	BANKSEL PORTC		; Select bank 0
	MOVLW	B'01000001'	; Analogue input setup code
	MOVWF	ADCON0		; f/8, RA0, done, enable 
	
	;PWM frequency 
	banksel STATUS; select bank 1
	movlw B'11111111' ; pwm freq = 0k, prescaler = 16 and fosc = 4M
	movwf PR2
	
	;PWM Duty
	banksel CCPR1L
	MOVLW B'00000000' 
	MOVWF CCPR1L 
	MOVLW B'00001100' 
	MOVWF CCP1CON
	
	;Initialize PWM PIN to OUTPUT
	banksel TRISC
	BCF TRISC, 2 ; set RC2 as OUTPUT, to use for PWM
	
	;; initialize TIMER 2 PRESCALE value * *
	banksel T2CON  ;select bank 0
	MOVLW B'00000111'
	MOVWF T2CON
	
	clrf TMR2 ;Clear TIMER 2 
	bsf T2CON, 2 ;Enable TIMER 2

	GOTO check

; Read ADC input and store ................................

getADC BSF ADCON0,GO	; start ADC..

waitHere 	BTFSC ADCON0,GO	; ..and wait for finish
			GOTO waitHere
			MOVF ADRESH,W	; store result high byte
			MOVWF val2
		RETURN	

; Main Loop ...............................................

check NOP

		;BTFSS PORTA,0 ;Check Input Button
		;GOTO S1
		BANKSEL PORTA
		call getADC; read input

FindLevel	
			movf ADRESH,val2
			movlw D'153'	; 3V/ (5/255)) = 153
			subwf val2
			btfsc STATUS,Z
		   	GOTO EqualOrLarger3	; VAL2=VAL1=3
		   	btfsc STATUS,C
		   	goto EqualOrLarger3; VAL2>VAL1 (VAL2>3)

		
			movf val2,ADRESH ; compare with two
			movlw D'102'	; 2V / (5/255) = 102
			subwf ADRESH
			btfsc STATUS,Z
		   	goto  LessEqThan2; VAL2=VAL=2
		   	btfsc STATUS,C
		   	goto  Equal2Less3; VAL2>VAL1 (2<Val2<3)
			;less than two then output nothing
			goto LessEqThan2  	;VAL<2

LessEqThan2 
				MOVLW 0x04
				MOVWF PORTB

				;PWM frequency 
				banksel STATUS; select bank 1
				movlw B'11111111' ; pwm freq = 0k, prescaler = 16 and fosc = 4M
				movwf PR2

				banksel CCPR1L
				MOVLW B'00000000' 
				MOVWF CCPR1L 
				MOVLW B'00001100' 
				MOVWF CCP1CON
				
				banksel TRISC
				BCF TRISC, 2 ; set RC2 as OUTPUT, to use for PWM
				
				banksel T2CON  ;select bank 0
				MOVLW B'00000111'
				MOVWF T2CON	 ;TIMER 2 PRESCALE value
			
				GOTO check			
			

EqualOrLarger3
				MOVLW 0x01
				MOVWF PORTB	 
				
				; SET PWM frequency  
				banksel STATUS; select bank 1
				movlw B'01010010' ; pwm freq = 3k, prescaler = 4 and fosc = 4M
				movwf PR2

				banksel CCPR1L
				MOVLW B'00001000' 
				MOVWF CCPR1L 
				MOVLW B'00011100' 
				MOVWF CCP1CON			;Set PWM Duty
				
				banksel TRISC
				BCF TRISC, 2 ; set RC2 as OUTPUT, to use for PWM

				banksel T2CON  ;select bank 0
				MOVLW B'00000101'
				MOVWF T2CON		;Set TIMER 2 PRESCALE value 

				GOTO check

Equal2Less3
			MOVLW 0x02
			MOVWF PORTB
			
				;SET PWM frequency 
			banksel STATUS; select bank 1
			movlw B'11111001' ; pwm freq = 1k, prescaler = 16 and fosc = 4M
			movwf PR2
			
			banksel CCPR1L
			MOVLW B'00011000' 
			MOVWF CCPR1L 
			MOVLW B'00111100' 
			MOVWF CCP1CON		;Set PWM Duty 
			
			banksel TRISC
			BCF TRISC, 2 ; set RC2 as OUTPUT, to use for PWM
				
			;PRESCALE = 16 SO THE PWM PERIOD = 1mS => PWM FREQUENCY = 1kHz
			banksel T2CON  ;select bank 0
			MOVLW B'00000101'
			MOVWF T2CON
			

			GOTO check


END ; Terminate source code
