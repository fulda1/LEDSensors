;;======================================================================;;
;;			Sfoukávací LED	 					;;
;;======================================================================;;
;;									;;
;; Program:		Hrátky s LED diodou - LED kterou lze sfouknout	;;
;; Code:		Jindra Fucik					;;
;; Platform:		Microchip PIC16F15313, 4 Mhz			;;
;; Date:		11.05.2022					;;
;; First release:	08.03.2021					;;
;; LastDate:		11.05.2022					;;
;;									;;
;;======================================================================;;

; Processor PIC 16F15313 running at 2MHz internal clock
;
; Revisions:
;
; 08/03/2022	Start of writing code.
; 11/05/2022	Playing with AD converter.

; ----- Definitions

#define		__VERNUM	.1
#define		__SUBVERNUM	.1
#define		__VERDAY	0x11
#define		__VERMONTH	0x05
#define		__VERYEAR	0x22


                list    p=16F15313,r=hex

		errorlevel -302
		;errorlevel -305
		;errorlevel -306

	        INCLUDE "P16F15313.INC"

; CONFIG1
; __config 0xFFEC
 __CONFIG _CONFIG1, _FEXTOSC_OFF & _RSTOSC_HFINT1 & _CLKOUTEN_OFF & _CSWEN_ON & _FCMEN_ON
; CONFIG2
; __config 0xF7FF
 __CONFIG _CONFIG2, _MCLRE_ON & _PWRTE_OFF & _LPBOREN_OFF & _BOREN_ON & _BORV_LO & _ZCD_OFF & _PPS1WAY_OFF & _STVREN_ON
; CONFIG3
; __config 0xFF9F
 __CONFIG _CONFIG3, _WDTCPS_WDTCPS_31 & _WDTE_OFF & _WDTCWS_WDTCWS_7 & _WDTCCS_SC
; CONFIG4
; __config 0xFFEF
 __CONFIG _CONFIG4, _BBSIZE_BB512 & _BBEN_OFF & _SAFEN_ON & _WRTAPP_OFF & _WRTB_OFF & _WRTC_OFF & _WRTSAF_OFF & _LVP_OFF
; CONFIG5
; __config 0xFFFF
 __CONFIG _CONFIG5, _CP_OFF


; ----- Macros

#define		DNOP		goto	$+1

#define		RAMINI0		0x020		; 80 bytes
#define		RAMINI1		0x0A0		; 80 bytes
#define		RAMINI2		0x120		; 80 bytes
#define		RAMINT		0x070		; 16 bytes	available in all banks

; ----- Constant values

FXTAL		equ	D'1000000'		; oscillator frequency

;         +---U---+
;    Vdd -|Vdd Vss|- Vss (GND)
;LED out -|RA5 RA0|- TxD
; ADC in -|RA4 RA1|- RxD
;        -|RA3 RA2|-
;         +-------+

; PMD = PERIPHERAL MODULE DISABLE
PMD0_INI	equ	b'00000101'	; CLKRMD CLKR enabled; SYSCMD SYSCLK enabled; FVRMD FVR enabled; IOCMD IOC disabled; NVMMD NVM disabled; 
PMD1_INI	equ	b'10000110'	; TMR0MD TMR0 enabled; TMR1MD TMR1 disabled; TMR2MD TMR2 disabled; NCOMD DDS(NCO) disabled; 
PMD2_INI	equ	b'01000111'	; ZCDMD ZCD disabled; CMP1MD CMP1 disabled; ADCMD ADC enabled; CMP2MD CMP2 disabled; DAC1MD DAC1 disabled; 
PMD3_INI	equ	b'00111111'	; CCP2MD CCP2 disabled; CCP1MD CCP1 disabled; PWM4MD PWM4 disabled; PWM3MD PWM3 disabled; PWM6MD PWM6 disabled; PWM5MD PWM5 disabled; 
PMD4_INI	equ	b'00010001'	; CWG1MD CWG1 disabled; MSSP1MD MSSP1 disabled; UART1MD EUSART enabled; 
PMD5_INI	equ	b'00011110'	; CLC3MD CLC3 disabled; CLC4MD CLC4 disabled; CLC1MD CLC1 disabled; CLC2MD CLC2 disabled; 

; PIN manager
LATA_INI    equ 0x00	    ; all outputs to zero

TRISA_INI   equ b'00011011' ; RA0,RA1 input - managed by charlieplexing, RA2 Not used, RA3 input - key buttons, RA4 not used, RA5 32768 Hz input

ANSELA_INI  equ b'00010000' ; RA4 is analog

WPUA_INI    equ	b'00001011'	; WPU for not used inputs

ODCONA_INI  equ	0x00	    ; disable open drain outputs
   
; oscillator management
OSCCON1_INI equ 0x60	    ; NOSC HFINTOSC; NDIV 1:1; 
;OSCCON2 is read only
OSCCON3_INI equ 0x00	    ; CSWHOLD may proceed; 
OSCEN_INI   equ 0x00	    ; MFOEN disabled; LFOEN disabled; ADOEN disabled; EXTOEN disabled; HFOEN disabled; 
OSCFRQ_INI  equ 0x02	    ; HFFRQ 4_MHz; 
OSCSTAT_INI equ 0x00	    ; MFOR not ready; do not understand, it is read only
;OSCTUNE_INI equ 0x20	    ; HFTUN 32; do not understand why MCC set this?
OSCTUNE_INI equ 0x00	    ; HFTUN 0; default

; Timer2 management			; Timer 2 is used for 1 sec pulses
;T2CLKCON_INI	equ 0x00    ; T2CS T2CKIPPS; 32768 Hz crystal used for 1 sec pulses
;T2HLT_INI   equ 0x00    ; T2PSYNC Not Synchronized; T2MODE Software control; T2CKPOL Rising Edge; T2CKSYNC Not Synchronized; 
;T2RST_INI   equ 0x00	; T2RSEL T2INPPS pin; 
;T2PR_INI    equ 0xFF	; T2PR 255 = 1 Hz; 
;T2CON_INI   equ b'11110000'	; T2CKPS 1:128; T2OUTPS 1:1; TMR2ON on; 

;T2INPPS_INI	equ 0x05	; RA5 is used as T2IN (that is default)

; Timer1 management
;T1GCON_INI	equ 0x00	; T1GE disabled; T1GTM disabled; T1GPOL low; T1GGO done; T1GSPM disabled; 
;T1GATE_INI	equ 0x00	; GSS T1G_pin; 
;T1CLK_INI	equ 0x01	; ClockSelect FOSC/4; 
;;TMR1H = 0x00;    //TMR1H 0; 
;;TMR1L = 0x00;    //TMR1L 0; 
;T1CON_INI	equ 0x14	; CKPS 1:2; nT1SYNC do_not_synchronize; TMR1ON disabled; T1RD16 disabled; 

; Timer0 management
T0CON1_INI	equ b'01010111'	; T0CS Fosc/4; T0CKPS 1:129; T0ASYNC not_synchronised; 
T0CON0_INI	equ b'10000000'	; T0OUTPS 1:1; T0EN enabled; T016BIT 8-bit; 
TMR0H_INI	equ .156	; in 8 bit mode the TMR0H is compared same as PR2 with timer2 --> 128 * 156 = 19968 us ~ 20 mili sec
;;TMR0L = 0x00; TMR0L 0; 

; EUSART management
BAUD1CON_INI	equ 0x08	; ABDOVF no_overflow; SCKP Non-Inverted; BRG16 16bit_generator; WUE disabled; ABDEN disabled; 
RC1STA_INI	equ 0x90	; SPEN enabled; RX9 8-bit; CREN enabled; ADDEN disabled; SREN disabled; 
TX1STA_INI	equ 0x24	; TX9 8-bit; TX9D 0; SENDB sync_break_complete; TXEN enabled; SYNC asynchronous; BRGH hi_speed; CSRC slave; 
SP1BRGL_INI	equ 0x03	; SP1BRGL 3; 250000 BPS
SP1BRGH_INI	equ 0x00	; SP1BRGH 0; 

RA2PPS_INI  equ 0x0F	    ; RA2->EUSART1:TX;
RA0PPS_INI  equ 0x00	    ; RA0->none

;ADC management
ADACT_INI	equ 0x00		; ADACT - External Trigger Disabled
ADCON1_INI	equ b'11000000'	; ADFM - Left justified; ADCS = FOSC/4 (1.0 us); ADPREF - VREF+ is connected to VDD
ADCON0_INI	equ b'00010001'	; CHS - RA4 is in; GOnDONE not in progress; ADCO enabled
;ADCON1_INI	equ b'11000011'	; ADFM - Left justified; ADCS = FOSC/4 (1.0 us); ADPREF - VREF+ is connected to FVR (2.048V)

;FVR management
FVRCON_INI	equ b'10000010'	; FVREN: - enabled; TSEN - disabled; TSRNG - 2VT; CDAFVR - off; ADFVR - 2048

; CLC 1 management
;CLC1CON_INI	equ	b'10000010'	; LC1EN is enabled, LC1INT disabled, LC1MODE is 4 input AND
;CLC1POL_INI	equ	b'00001100'	; LC1G1 not inverted, LC1G2 not inverted, LC1G3 inverted, LC1G4 inverted, LC1POL not invrted
;CLC1SEL0_INI	equ	b'00000000'	; data 0 is CLCIN0PPS
;CLC1SEL1_INI	equ	b'00000100'	; data 1 is Fosc
;CLC1SEL2_INI	equ	b'00000000'	; data 2 is CLCIN0PPS
;CLC1SEL3_INI	equ	b'00000000'	; data 3 is CLCIN0PPS
;CLC1GLS0_INI	equ	b'00000010'	; gate 0 input is data 0 non inverted
;CLC1GLS1_INI	equ	b'00001000'	; gate 0 input is data 1 non inverted
;CLC1GLS2_INI	equ	b'00000000'	; gate 2 input is none
;CLC1GLS3_INI	equ	b'00000000'	; gate 3 input is none

;INTPPS_INI		equ	b'00000010'	; map INT to RA2
;CLCIN0PPS_INI	equ	b'00000010'	; map CLCIN0 to RA2

; Interrupt section
PIE0_INI	equ 0x00	; TMR0 interrupt
PIE1_INI	equ 0x00	; none used
PIE2_INI	equ 0x00	; none used
PIE3_INI	equ 0x00	; none used
PIE4_INI	equ 0x00	; none used
PIE5_INI	equ 0x00	; none used
PIE6_INI	equ 0x00	; none used
PIE7_INI	equ 0x00	; none used

INTC_INI	equ 0x00	; GIE disable, PIE enable, falling edge of INT


; --- EEPROM Section - no EEPROM, it is emulated by SAF
#define		SAF1_INI	0x00780

; ----- Variables

; --- Internal RAM Section
		cblock  RAMINT
ADDATH		; data from AD converter High bits   \
ADDATM		; data from AD converter Middle bits -- cumulative data from AD converter
ADDATL		; data from AD converter Low bits    /
ADCNT		; counter for AD read cycles
		endc

		cblock  RAMINI0

        R3,R2,R1,R0        ;MUST START ON BINARY XXXXX000.
        COUNT, LBYTE, MBYTE, HBYTE

       endc


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
	BANKSEL	PORTA		; BANK 0
	movlw	LATA_INI	; all outputs to zero
	movwf	LATA
	movlw	TRISA_INI	; RA2 is output, RA3 is MCLR
	movwf	TRISA

	; PMD = PERIPHERAL MODULE DISABLE
	BANKSEL	PMD0		; BANK 15
	movlw	PMD0_INI	; CLKRMD CLKR enabled; SYSCMD SYSCLK enabled; FVRMD FVR disabled; IOCMD IOC enabled; NVMMD NVM enabled; 
	movwf	PMD0
	movlw	PMD1_INI	; TMR0MD TMR0 enabled; TMR1MD TMR1 enabled; TMR2MD TMR2 enabled; NCOMD DDS(NCO) enabled; 
	movwf	PMD1
	movlw	PMD2_INI	; ZCDMD ZCD disabled; CMP1MD CMP1 enabled; ADCMD ADC disabled; CMP2MD CMP2 disabled; DAC1MD DAC1 disabled; 
	movwf	PMD2
	movlw	PMD3_INI	; CCP2MD CCP2 disabled; CCP1MD CCP1 disabled; PWM4MD PWM4 disabled; PWM3MD PWM3 disabled; PWM6MD PWM6 disabled; PWM5MD PWM5 disabled; 
	movwf	PMD3
	movlw	PMD4_INI	; CWG1MD CWG1 disabled; MSSP1MD MSSP1 disabled; UART1MD EUSART enabled; 
	movwf	PMD4
	movlw	PMD5_INI	; CLC3MD CLC3 disabled; CLC4MD CLC4 disabled; CLC1MD CLC1 disabled; CLC2MD CLC2 disabled; 
	movwf	PMD5

; PIN manager
	BANKSEL	ANSELA		; BANK 62

	movlw	ANSELA_INI	; RA0 and RA1 to analog = comparator inuts
	movwf	ANSELA
	movlw	WPUA_INI	; disable WPU
	movwf	WPUA
	movlw	ODCONA_INI	; disable open drain outputs
	movwf	ODCONA

	movlw	RA0PPS_INI	; RA0->GPIO;
	movwf	RA0PPS
	movlw	RA2PPS_INI      ; RA2->EUSART2:TX;
	movwf	RA2PPS

; oscilator management
	BANKSEL	OSCCON1		; BANK 17
	movlw	OSCCON1_INI	; NOSC HFINTOSC; NDIV 4; 
	movwf	OSCCON1
		;OSCCON2 is read only
	movlw	OSCCON3_INI	; CSWHOLD may proceed; 
	movwf	OSCCON3
	movlw	OSCEN_INI	; MFOEN disabled; LFOEN disabled; ADOEN disabled; EXTOEN disabled; HFOEN disabled; 
	movwf	OSCEN
	movlw	OSCFRQ_INI	; HFFRQ 8_MHz; 
	movwf	OSCFRQ
	movlw	OSCSTAT_INI	; MFOR not ready; do not understand, it is read only
	movwf	OSCSTAT
	movlw	OSCTUNE_INI	; HFTUN 32; do not understand why MCC set this?
	movwf	OSCTUNE

; EUSART management
	BANKSEL	BAUD1CON
	movlw	BAUD1CON_INI	; ABDOVF no_overflow; SCKP Non-Inverted; BRG16 16bit_generator; WUE disabled; ABDEN disabled; 
	movwf	BAUD1CON
	movlw	RC1STA_INI	; SPEN enabled; RX9 8-bit; CREN enabled; ADDEN disabled; SREN disabled; 
	movwf	RC1STA
	movlw	TX1STA_INI	; TX9 8-bit; TX9D 0; SENDB sync_break_complete; TXEN enabled; SYNC asynchronous; BRGH hi_speed; CSRC slave; 
	movwf	TX1STA
	movlw	SP1BRGL_INI	; SP1BRGL 3; 250000 BPS
	movwf	SP1BRGL
	movlw	SP1BRGH_INI	; SP1BRGH 0; 
	movwf	SP1BRGH

; analog to digital management
	BANKSEL	ADCON0
	movlw	ADACT_INI	; ADACT - External Trigger Disabled
	movwf	ADACT
	movlw	ADCON1_INI	; ADFM - Left justified; ADCS = FOSC/4 (1.0 us); ADPREF - VREF+ is connected to VDD
	movwf	ADCON1
	movlw	ADCON0_INI	; CHS - RA4 is in; GOnDONE not in progress; ADCO enabled
	movwf	ADCON0

; Timer0 management
	BANKSEL	T0CON1
	movlw	T0CON1_INI	; T0CS Fosc/4; T0CKPS 1:129; T0ASYNC not_synchronised; 
	movwf	T0CON1
	movlw	T0CON0_INI	; T0OUTPS 1:1; T0EN enabled; T016BIT 8-bit; 
	movwf	T0CON0
	movlw	TMR0H_INI	; in 8 bit mode the TMR0H is compared same as PR2 with timer2 --> 128 * 156 = 19968 us ~ 20 mili sec
	movwf	TMR0H

; Interrupts
	BANKSEL	PIE0	; BANK 2
	movlw	PIE0_INI	; Enable TMR0 interrupt set TMR0IE = 1.
	movwf	PIE0
	movlw	PIE1_INI	; none used
	movwf	PIE1
	movlw	PIE2_INI	; Enabling CMP1 interrupt.
	movwf	PIE2
	movlw	PIE3_INI	; enable receive interrupt
	movwf	PIE3
	movlw	PIE4_INI	; Enabling TMR2 interrupt.
	movwf	PIE4
	movlw	PIE5_INI	; none used
	movwf	PIE5
	movlw	PIE6_INI	; none used
	movwf	PIE6
	movlw	PIE7_INI	; none used
	movwf	PIE7

	movlb	0	; BANK 0
	movlw	RAMINI0			; Clear variables bank0
	movwf	FSR0L
	clrf	FSR0H
	clrw
ClearRAM:
	movwi	FSR0++
	;clrf	INDF0
	;incf	FSR0L,f
	btfss	FSR0L,7			; to address 7F
	goto	ClearRAM

	movlb	0	; BANK 0
	movlw	INTC_INI		; Set interrupts
	movwf	INTCON


MainLoop:
	BANKSEL	PIR0	; BANK 14
	btfsc	PIR0, TMR0IF
	call	Measure
	
	goto	MainLoop
	
Measure:
	bcf	PIR0, TMR0IF
	clrf	ADDATH
	clrf	ADDATM
	clrf	ADDATL
	clrf	ADCNT
	BANKSEL	ADCON0		; BNK 1
	bsf		ADCON0,GOnDONE
	nop
ADCloop:
	movf	ADRESL,w
	addwf	ADDATL,f
	btfss	STATUS,C
	goto	AddMiddle
	incfsz	ADDATM,f
	goto	AddMiddle
	incf	ADDATH,f
AddMiddle
	movf	ADRESH,w
	addwf	ADDATM,f
	btfsc	STATUS,C
	incf	ADDATH,f

	bsf		ADCON0,GOnDONE
	decfsz	ADCNT,f
	goto	ADCloop

	movlb	0
	movf	ADDATL,w
	;movlw	2
	movwf	LBYTE
	movf	ADDATM,w
	;clrw
	movwf	MBYTE
	movf	ADDATH,w
	movwf	HBYTE
	call	bin2dec

	movlw	R0
	movwf	FSR0L
	movlw	4
	movwf	ADCNT
	BANKSEL	TX1REG
TxMsgLoop
	swapf	INDF0,w
	andlw	0x0F
	addlw	'0'
	movwf	TX1REG
TXbytes1
	btfss	TX1STA,TRMT
	goto	TXbytes1

	moviw	FSR0--
	andlw	0x0F
	addlw	'0'
	movwf	TX1REG
TXbytes2
	btfss	TX1STA,TRMT
	goto	TXbytes2
	decfsz	ADCNT,f
	goto	TxMsgLoop

	movlw	'\n'
	movwf	TX1REG
TXcr
	btfss	TX1STA,TRMT
	goto	TXcr

	movlw	'\r'
	movwf	TX1REG
;TXlf
;	btfss	TX1STA,TRMT
;	goto	TXlf
	return

;	http://piclist.com/techref/microchip/math/radix/b2bu-24b9d-xx.htm
;	convert 24 bit number to 8 decimal numbers - unpacked
;	input bin0 - bin2
;	output dec0 - dec7

bin2dec:
	MOVLW   .24 ;0x18
        MOVWF   COUNT
        CLRF    R0
        CLRF    R1
        CLRF    R2
        CLRF    R3
	clrf	FSR0H
        BCF     STATUS,C
        GOTO    BIN2BC2
BIN2L   MOVLW   R3
        MOVWF   FSR0L
BCDADJ  MOVLW   03H
        ADDWF   INDF0,F
        BTFSS   INDF0,3
        SUBWF   INDF0,F
        MOVLW   30H	; 48
        ADDWF   INDF0,F
        BTFSS   INDF0,7
        SUBWF   INDF0,F
        INCF    FSR0L,F
        BTFSS   FSR0L,2
        GOTO    BCDADJ
BIN2BC2 RLF     LBYTE,F
        RLF  MBYTE,F
        RLF     HBYTE,F
        RLF R3,F
        RLF     R2,F
        RLF     R1,F
        RLF     R0,F
        DECFSZ  COUNT,F
        GOTO    BIN2L
        RETURN


; ----------------------------------------------------------------------
	org	SAF1_INI

		dt	"CharliPX"
		dt	" J.Fucik"
		dt	(__VERDAY   >> 4)  +0x30
		dt	(__VERDAY   & 0x0F)+0x30,"/"
		dt	(__VERMONTH >> 4)  +0x30
		dt	(__VERMONTH & 0x0F)+0x30,"/"
		dt	(__VERYEAR  >> 4)  +0x30
		dt	(__VERYEAR  & 0x0F)+0x30

		end
