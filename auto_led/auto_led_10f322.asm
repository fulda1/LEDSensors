;;======================================================================;;
;;			LED reagujici na svetlo	 					;;
;;======================================================================;;
;;									;;
;; Program:		Hrátky s LED diodou - LED ktera sviti jen kdyz je tma	;;
;; Code:		Jindra Fucik					;;
;; Platform:		Microchip PIC10F320(2), 500 khz			;;
;; Date:		23.05.2022					;;
;; First release:	08.03.2021					;;
;; LastDate:		11.05.2022					;;
;;									;;
;;======================================================================;;

; Processor PIC 10F322 running at 500 kHz internal clock
;
; Revisions:
;
; 08/03/2022	Start of writing code.
; 11/05/2022	Playing with AD converter.
; 23/05/2022	Playing with CCP + Timer
; 26/05/2022	Convert to 10F320(2)

; ----- Definitions

#define		__VERNUM	.1
#define		__SUBVERNUM	.3
#define		__VERDAY	0x26
#define		__VERMONTH	0x05
#define		__VERYEAR	0x22

#define		DARK	0x62				; my view of dark
							; 0x75 * 256 = 0x7500 = 29952 * 8 = 239616 micro sec
							; 0x62 * 256 = 0x6200 = 25088 * 8 = 200704 micro sec



	IFDEF __10F322
		list    p=10F322,r=hex
		INCLUDE "P10F322.INC"
	ENDIF

	IFDEF __10F320
		list    p=10F320,r=hex
		INCLUDE "P10F320.INC"
	ENDIF

                __FUSES _BOREN_OFF & _CP_OFF & _PWRTE_ON & _WDTE_OFF & _LVP_OFF & _MCLRE_ON  & _FOSC_INTOSC & _WRT_BOOT

#define		RAMINI0		0x040		; 64 bytes

; ----- Macros

#define		DNOP		goto	$+1


; ----- Constant values

FXTAL		equ	D'500000'		; oscillator frequency

;           +---U---+
;          -|RA0 RA3|-
;Vss (GND) -|Vss Vdd|- Vdd
;LED minus -|RA1 RA2|- LED plus
;           +-------+

;           +---U---+
;          -|NC  RA3|-
;      Vdd -|Vdd Vss|- Vss (GND)
; LED plus -|RA2  NC|- 
;LED minus -|RA1 RA0|-
;           +-------+

#define		LED_PLUS	2	; plus pole of LED
#define		LED_MINUS	1	; minus pole of LED


;RA_TRIS         equ     0x0C			; RA0: out led, RA1 out led, RA2 in dcc, RA3 in addr
RA_TRIS         equ     b'00001001'		; not connected as inputs
RA_INI          equ     0x00			; all zero
OPTION_INI	equ	0x07			; Option register: enable pull-up, prescaler 1:256
;bit7	1=weak pull up disable 0=WPU enable
;bit6	0=int on falling edge 1=rising
;bit5	0=TMR0 internal clock 1=T0CLKI pin
;bit4	0=TMR0 increment low->high 1=high->low
;bit3	1=prescaler is disabled 0=prescaler is enabled
;bit2-0 prescaler

;INTC_INI	equ	0x90			; GIE, INTE enable, PEIE disable
INTC_INI	equ	0x00			; no interrupts
PIE1_INI	equ	0x00			; no interrupts
WPUA_INI	equ 0x09			; weak pull-ups on not connected inputs
;T2CON_INI 	equ	0x0D			; Prescaler 1:4, Timer 2 enable, Postscaler 1:2
;PR2_INI		equ	0xF9			; 249 dec = 250 instruction = 1ms with prescaler
;T2CON_INI 	equ	0x05			; Prescaler 1:4, Timer 2 enable
;PR2_INI		equ	0x7C			; 124 dec = 125 instruction = 0,5ms with prescaler
T2CON_INI 	equ	0x00			; off
OSCCON_INI	equ b'00100000'		; 500 kHz


; ----- Variables

; --- Internal RAM Section
		cblock  RAMINI0
TmrRes		; CCP conversion result
Flags		; light flags
COUNT		; counter for light time
		endc

#define	    LedOn   Flags,0	; flag for lighting

; ------ Program Section

		org	0x000

PowerUp:
		;clrf	STATUS			; ensure we are at bank 0
		clrf	PCLATH			; ensure page bits before goto !!
		clrf	INTCON			; disable all interrupts
		goto	Start


;
;**********************************************************************************************************************
; ISR (Interrupt Service Routines)
;**********************************************************************************************************************
;
	org	0x004
Interrupt:
	retfie


Start:
	clrf	LATA
	clrf	ANSELA			; all digital
	movlw   RA_TRIS         	; Set port A I/O configuration
	movwf   TRISA
	movlw	WPUA_INI
	movwf	WPUA

	movlw	OSCCON_INI		; 500 kHz
	movwf	OSCCON

	movlw	OPTION_INI		; Option register: no pull-up, no prescaler, wdt 1:1
	movwf	OPTION_REG
	movlw	PIE1_INI	; no interrupts
	movwf	PIE1
	movlw	T2CON_INI 	; off
	movwf	T2CON

	bsf	OPTION_REG,PS2
	bsf	OPTION_REG,PS1
	bsf	OPTION_REG,PS0
	bsf	OPTION_REG,PSA
	bcf	OPTION_REG,PSA

	movlw	INTC_INI		; Set interrupts
	movwf	INTCON

	bcf		LATA,LED_PLUS	; reverse energize of LED
	bsf		LATA,LED_MINUS	; 
	bcf LedOn
	clrf	TMR0

StartMeasure:
	movlw	.25				; 25 * 5 = 125 ticks --> 1 ms
	movwf	COUNT
EnergizeLoop
	DNOP					; 1,2
	decfsz	COUNT,f			; 3
	goto	EnergizeLoop	; 4,5
	bsf		TRISA,LED_PLUS
	clrf	TMR0			; reset timer
	bsf	OPTION_REG,PSA
	bcf	OPTION_REG,PSA
	bcf		INTCON,TMR0IF
DetectLoop
	btfsc	INTCON,TMR0IF
	goto	RollOver		; timer is over = we are in dark
	btfss	PORTA,LED_PLUS	; is LED de-energized?
	goto	DetectLoop
	movf	TMR0,w
	sublw	DARK			; compare with dark
	btfss	STATUS,C
	goto	TurnLEDon
	bcf		TRISA,LED_PLUS
	goto	StartMeasure

RollOver:
TurnLEDon:
	bcf		TRISA,LED_PLUS
	bsf		LATA,LED_PLUS	; Turn LED on
	bcf		LATA,LED_MINUS	; 

	movlw	.4
	movwf	COUNT
	clrf	TMR0			; reset timer
	bcf		INTCON,TMR0IF

LightLoop
	btfss	INTCON,TMR0IF	; is timer over?
	goto	LightLoop
	bcf		INTCON,TMR0IF
	decfsz	COUNT,f			; wait ~ 2 sec
	goto	LightLoop
	
	bcf		LATA,LED_PLUS	; reverse energize of LED
	bsf		LATA,LED_MINUS	; 
	goto	StartMeasure	; and again




; ----------------------------------------------------------------------
	IFDEF __10F322
		org	0x01E0
	ENDIF

	IFDEF __10F320
		org	0x00E0
	ENDIF

		dt	"Auto LED"
		dt	" J.Fucik"
		dt	(__VERDAY   >> 4)  +0x30
		dt	(__VERDAY   & 0x0F)+0x30,"/"
		dt	(__VERMONTH >> 4)  +0x30
		dt	(__VERMONTH & 0x0F)+0x30,"/"
		dt	(__VERYEAR  >> 4)  +0x30
		dt	(__VERYEAR  & 0x0F)+0x30

		end
