;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
;	Title:		Music Generator
;	Author:		Sashen Govender
;	Date:		13 October 2012
;	Version:	1.0
;	File Name:	210513093P5.asm

;------------------------------------------------------------------------------------------------------------
;*********************Overview*********************Overview*********************Overview*********************
;------------------------------------------------------------------------------------------------------------
;	This Program is capable of generating a sinusoidal music tone based on a table of notes. The sine 
;	wave that is synthesized via PICs PWM module is generated using 64 different PWM levels. This output 
;	waveform is then sent through a Low pass Filter, which in turn should make the signal a nearly 
;	perfect sine wave, and finally to a speaker that plays the tune continuously. 31 different
;	notes can be played and these notes range from 110Hz (1A) to 622Hz(D#) including sharps.

;------------------------------------------------------------------------------------------------------------
;******************Table of Notes****************Table of Notes****************Table of Notes****************
;------------------------------------------------------------------------------------------------------------
;	This program can play a Symphony by simply changing the SymphonyTable. This table contains the 
;	musical data required to generate the Symphony. The Symphony table must not contain more than 195
;	notes (Otherwise the SymphonyTable will cross over to the next page) and the last entry in the
;	table be 0 (retlw 0x00). If these two conditions are not met, the output tone will be incorrect.

;	The elements in the SymphonyTable must follow the following format. The upper three bits contain the
;	note duration which can range from 125ms (001) to 875ms (111) and the lower 5 bits act as an index
;	for the music notes. E.g. A musical note, 3E, which is required to be played for 375mS would be
;	represented by 0x68. Please note that an index of 00000 indicates that no sound will be produced. 

;	The following table summarizes all the notes this program can play along with the required note index
;	-------------------------------
;	|Note  |Music	|Octave	|Freq	|
;	|Index |Note 	|	|	|
;	-------------------------------
;	|00000	|None	|None	|None	|
;	|00001	|A	|2	|110	|
;	|00010	|A#	|2	|117	|
;	|00011	|B	|2	|124	|
;	|00100	|C	|3	|131	|
;	|00101	|C#	|3	|139	|
;	|00110	|D	|3	|147	|
;	|00111	|D#	|3	|156	|
;	|01000	|E	|3	|165	|
;	|01001	|F	|3	|175	|
;	|01010	|F#	|3	|185	|
;	|01011	|G	|3	|196	|
;	|01100	|G#	|3	|208	|
;	|01101	|A	|3	|220	|
;	|01110	|A#	|3	|233	|
;	|01111	|B	|3	|247	|
;	|10000	|C	|4	|262	|
;	|10001	|C#	|4	|277	|
;	|10010	|D	|4	|294	|
;	|10011	|D#	|4	|311	|
;	|10100	|E	|4	|330	|
;	|10101	|F	|4	|349	|
;	|10110	|F#	|4	|370	|
;	|10111	|G	|4	|392	|
;	|11000	|G#	|4	|415	|
;	|11001	|A	|4	|440	|
;	|11010	|A#	|4	|466	|
;	|11011	|B	|4	|494	|
;	|11100	|C	|5	|523	|
;	|11101	|C#	|5	|554	|
;	|11110	|D	|5	|587	|
;	|11111	|D#	|5	|622	|
;	-------------------------------

;------------------------------------------------------------------------------------------------------------
;************Tone Implementation*************Tone Implementation*************Tone Implementation*************
;------------------------------------------------------------------------------------------------------------
;	This program makes uses of two interrupts: Timer1 and Timer2 interrupts.
;
;	1. Timer1 interrupt was used to read the next musical value in the SymphonyTable. The value read from 
;	this table contains the musical note and duration of the note to be played. Timer1 made to overflow
;	every 125mS but a new musical value is only read after the current musical note duration has been
;	achieved. 
;
;	2. Timer2 interrupt was used to read all the values in the correct SineTable and continues to read
;	these values until a Timer1 overflow interrupt has occurred.
;
;	This program is structured such that Timer1 has a higher priority than Timer2. Once all the musical
;	values in the SymphonyTable has been read, the table is read again indefinitely. 

;------------------------------------------------------------------------------------------------------------
;**************Program Structure**************Program Structure**************Program Structure***************
;------------------------------------------------------------------------------------------------------------
;	1. Reset Vector:
;		1.1. Set clock frequency to 4 MHz
;		1.2. Initialise output PWM pin
;		1.3. Setup PWM Module and Timer2
;		1.4. Setup Timer1 for Interrupts after 125mS

;	2. Proceed to main:
;		2.1. Infinite Loop waiting for interrupts to occur

;	3. Check which interrupt has occurred:
;		3.1. Save W and Status Registers
;		3.2. Timer1 overflow	=>Read new music value (From the SymphonyTable) to play
;		3.3. Timer2 to PR2	=>Load the next sine sampling value (from the correct SineTable)

;	4. Timer1 Interrupt:
;		4.1. Clear Timer1 interrupt Flag
;		4.2. Has the required note duration been achieved?
;			4.2.1. No=>	4.2.1.1. Increment overflow counter
;					4.2.1.2. Re-configure Timer1
;
;			4.2.2. Yes=>	4.2.2.1. Read the next music value from SymphonyTable
;					4.2.2.2. Has the music data read returned a zero value?
;						4.2.2.2.1. Yes	=> Re-read SymphonyTable from the beginning	
;						4.2.2.3.2. No	=> Continue with program as normal
;					4.2.2.3. Extract the new note duration (125mS-875mS)
;					4.2.2.4. Extract the new music note value (0-31)
;					4.2.2.5. Use the music note value to obtain the correct PR2
;						  value required to generate the sine frequency. 
;					4.2.2.6. Determine which one of the 13*[N1] SineTables to call
;					4.2.2.7. Obtain the correct SineTable value (Sinevalue) 
;					4.2.2.8. Update the CCPR1L register
;					4.2.2.9. Update the PR2 register
;					4.2.2.10. Increment the SymphonyTable position 
;					4.2.2.11. Increment the Sine Sample position (Read the next sine value 
;						  in the SineTable)
;					4.2.2.12. Increment Overflow counter
;					4.2.2.13. Re-configure Timer1
;		4.3. Load the original W and Status register and exit ISR

;	5. Timer2 Interrupt:
;		5.1. Clear Timer2 interrupt Flag
;		5.2. Determine if the Sine sampling position value is between 0-31 or 32-63
;			5.2.1. Between 0-31	=>Read the SineTable normally 
;			5.2.2. Between 32-63	=>5.3.2.1. Read the SineTable normally 
;						  5.3.2.2. Subtract the returned value from the PR2Value*[N2]
;		5.3. Increment the SineSamplePoint position (read the next value in the SineTable)
;		5.4. Check if SineSamplePoint has reached a value of 64
;			5.4.1. Yes	=>Reset the SineSamplePoint (read the SineTable from the beginning)
;			5.4.2. No	=>Proceed as normal
;
;		5.5. Load the original W and Status register before the interrupt

;	Notes:
;		[N1]
;		Although this program can play 32 different frequencies including no sound, it requires only
;		13 SineTables instead of 32.This is due to the relationship that exists between music octaves.
;		(The frequency of the music note in an octave is twice frequency of the same note in the
;		previous octave and half the frequency in the next octave. e.g. 2A has frequency 110Hz and 3A 
;		has frequency 220Hz and 4A has frequency 440Hz). I have 12 tables that store the first 12 note
;		frequencies out of the 32 required frequencies and 1 table for no note. Since the frequencies
;		of the other notes are 2times or 4times the stored frequency, I need to divide the CCPR1L 
;		value by either 2 or 4 depending on the required musical note.

;		[N2]
;		The Sine Function is a symmetrical function, whereby the points in each quadrant are related.
;		Therefore, instead of storing 64 points in my SineTable, I required only 32 points, (1/2 
;		SineTable) relating to 0-180 degrees. I then reuse these 32 points in a specific way
;		to create the other half(270-360 degrees)of the sine wave.
;
;		SineValue represents the Duty Cycle of the PWM frequency
;------------------------------------------------------------------------------------------------------------
;*****************Important Info****************Important Info****************Important Info*****************
;------------------------------------------------------------------------------------------------------------
;	Files required:	None
;	Clock Speed:		4 MHz
;	Microchip:		PIC16F690
;	Macros:			None
;	Interrupts:		Timer1 Overflow, Timer2 to PR2 interrupt 
;	Timer usage:		Timer1, Timer2 
;
;	Code Performance Figures:
;	-------------------------
;	Program memory:	852
;	Max Stack Depth:	5


;------------------------------------------------------------------------------------------------------------
;***************Code Improvements**************Code Improvements**************Code Improvements**************
;------------------------------------------------------------------------------------------------------------
;	1. 32 stored Sine points for a 64 point sine sample
;		This can be reduced to only 16 points by exploiting the symmetry of Sine wave even further.
;		(Use 16 points to represent the first quadrant of the sine wave, then apply some logic to
;		create the rest of the sine wave).

;	2. NoSine Table
;		This is just a table of zero used to generate no sound. This table can be eliminated and
;		some additional code can be added to carter for this removal.

;------------------------------------------------------------------------------------------------------------
;***************Pin Connections****************Pin Connections****************Pin Connections****************
;------------------------------------------------------------------------------------------------------------
;	Pin 1 (Vdd):		Vdd = 5V 
;	Pin 5 (CCP1):		PWM output pin
;	Pin 20 (Vss):		Vss = 0V

;------------------------------------------------------------------------------------------------------------
;***********Configuration Section***********Configuration Section***********Configuration Section************
;------------------------------------------------------------------------------------------------------------

list p=16F690
#include <p16f690.inc>

__CONFIG _CP_OFF & _CPD_OFF & _BOR_OFF & _MCLRE_ON & _WDT_OFF & _PWRTE_ON &_INTRC_OSC_NOCLKOUT & _FCMEN_OFF & _IESO_OFF
;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------

cblock 0x72

	W_Save			; Save the W Reg while in the ISR
	STATUS_Save		; Save the Status Reg while in the ISR
	SineSamplePosition	; Contains a value between 0 and 63, which refers to the position in my 
				; "64 point Sine Sample ".
	SineTablePoint		; Contains the position (0-31) of the next SineValue to load from the SineTable
	SineValue		; Store the value returned from the respective SineTable
	MusicByte		; store the value returned from the SymphonyTable
	NoteDuration		; Contains the duration of the music note (value 1 - 7)
	MusicNote		; Store a value between 0 and 31 that is used to decide on which music note to 
				; play (i.e. Note Index)
	SymphonyPosition	; Stores the position of the next music value to load from the SymphonyTable
	Timer1_Overflows	; counts the number of Timer1 overflows
	MusicTableSelect	; Indicates which SineTable to call in my SineTables subroutine 
	Divider			; Stores a value between 0-2, that is used to decide on the value the 
				; SineValue must be divided by.
	PR2Value		; contains the PR2 value returned from the MusicNotePR2 Table
endc
;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
;	Constant definitions
	PWM_Out_Port 	equ 	TRISC	; port 	
	PWM_Out_bit 	equ 	5	; and bit for the PWM output signal 

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------

org 0x00
	pagesel Initialise
	goto Initialise

org 0x04
	pagesel ISR
	goto ISR

;------------------------------------------------------------------------------------------------------------
;********************Initialise*******************Initialise*******************Initialise********************
;------------------------------------------------------------------------------------------------------------
;	Code to setup the:	clock frequency to 4 MHz
;				Initialise output PWM pin
;				PWM Module and Timer2
;				Timer1 for Interrupts after 125mS
Initialise:
	banksel	OSCCON
	movlw	b'01100101'
					; Bit6-4:	110	=> 4 MHz
					; Bit3:		0	=> Internal Oscillator
					; Bit2:		1	=> High Frequency Stable (enabled)
					; Bit0:		1	=> Internal Oscillator for system clock
	movwf	OSCCON

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
	bcf	PWM_Out_Port,PWM_Out_bit ; PWM output pin

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
	banksel	CCP1CON
	movlw	b'00001100'
					; Bit7-6:	00	=> single output
					; Bit3-0:	1100	=> PWM Mode P1A active-high
	movwf	CCP1CON
	
	movlw	b'00000000'		; Bit2:		0	=> Stop Timer2
					; Bit1-0:	00	=> Prescaler 1
	movwf	T2CON
	clrf	TMR2
		
	bcf	PIR1,TMR2IF		; Clear Timer2 Interrupt flag
	banksel	PIE1
	bsf	PIE1,TMR2IE		; Enable TMR2 Interrupts

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
	banksel INTCON
	bsf	INTCON,PEIE		; Enable timer1 and Timer2 interrupts in PEIE
	bsf	INTCON,GIE		; Enable interrupts to occur

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
	call	Config_Timer1		; Configure Timer1 for 125mS interrupt

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
	clrf	SymphonyPosition	; Reset the Symphony Table position

;------------------------------------------------------------------------------------------------------------
;************************Main************************Main************************Main************************
;------------------------------------------------------------------------------------------------------------
;	Infinite loop waiting for interrupts to occur
Main:
	goto	Main

;------------------------------------------------------------------------------------------------------------
; *****************Config_Timer1*****************Config_Timer1*****************Config_Timer1*****************
;------------------------------------------------------------------------------------------------------------
;	Configure Timer1 for a 125mS overflow
;	((clock)/4) ^-1)*(Timer Prescaler)*(2^16-TMR1 Value)=Time overflow
;	((4*10^6)/4) ^-1)*8*((2^16)-15625)=125mS

Config_Timer1:
	banksel	T1CON
	movlw	b'00110000'
					; Bit5-4:	11	=> Prescaler 1:8
					; Bit1:		0	=> Internal Clock
					; Bit0:		0	=> Stop Timer1
	movwf	T1CON
	movlw	0xC2
	movwf	TMR1H
	movlw	0xF7			; 125mS
	movwf	TMR1L

	bcf	PIR1,TMR1IF		; clear overflow flag bit
	banksel	PIE1
	bsf	PIE1,TMR1IE		; timer1 overflow interrupt
	banksel	T1CON
	bsf	T1CON,0			; Start Timer1
	return

;------------------------------------------------------------------------------------------------------------
;*************************ISR*************************ISR*************************ISR************************
;------------------------------------------------------------------------------------------------------------
ISR:
;	Save W and Status registers
	movwf	W_Save
	movf	STATUS,W
	movwf	STATUS_Save

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
;	Determine which Interrupt had occurred and call the correct Timer ISR
	banksel	PIR1
	btfsc	PIR1,TMR1IF		; check if Timer1 interrupt had occurred
	call	Timer1_ISR
	btfsc	PIR1,TMR2IF		; check if Timer2 interrupt had occurred
	call	Timer2_ISR

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
;	Load the original value of the STATUS and W registers
	movf	STATUS_Save,W
	movwf	STATUS
	swapf	W_Save,F
	swapf	W_Save,W
	retfie

;------------------------------------------------------------------------------------------------------------
;*******************Timer1_ISR*******************Timer1_ISR*******************Timer1_ISR*********************
;------------------------------------------------------------------------------------------------------------
;	Description:	Timer1 interrupt used to read the next musical note in the SymphonyTable and overflows
;			every 125mS.

;	No. of Instructions:	37  
;	Stack Depth :		4
;	Flags Used :		C
;------------------------------------------------------------------------------------------------------------
Timer1_ISR:
	bcf	PIR1,TMR1IF		; Clear the Timer1 overflow flag bit
	movf	NoteDuration,W
	subwf	Timer1_Overflows,W
	btfsc	STATUS,C		; Check if the required note duration has been achieved. 
	goto	NewNote			; Note Duration has been achieved, Therefore read new note
	incf	Timer1_Overflows,F	; Note Duration not yet up
	call	Config_Timer1		; Re-configure Timer1
	goto 	End_Timer1

;------------------------------------------------------------------------------------------------------------
;	Read the next piece of musical data from the SymphonyTable which contains the musical note and 
;	duration of the note all in one value

NewNote:
	call	ReadNewNote

;------------------------------------------------------------------------------------------------------------
;	Check if the value returned from the SymphonyTable is a zero. If so, the entire table has been read.	
	movlw	D'0'
	bcf	STATUS,Z
	subwf	MusicByte,W
	btfsc	STATUS,Z
	clrf	SymphonyPosition	; Entire SymphonyTable has been read. Reset SymphonyTable offset
	btfsc	STATUS,Z
	call	ReadNewNote		; Re-read the SymphonyTable from the begin

;------------------------------------------------------------------------------------------------------------
	call	FindDuration		; Find the musical note duration
	call	FindMusicNote		; Find the musical note to play
	
	movlw	HIGH MusicNotePR2
	movwf	PCLATH
	call	MusicNotePR2		; Use the musical note value as an offset in the MusicNotePR2 
					; table, to get the PR2 value required to create the Sine wave
	movwf	PR2Value		; and store this returned value.
	movlw	HIGH ISR
	movwf	PCLATH
	clrf	SineSamplePosition	; Reset the Sine Sampling Point 
	clrf	SineTablePoint		; Reset the offset location in the SineTable
	call	FirstSineValue		; Find the first SineValue in the correct SineTable

;------------------------------------------------------------------------------------------------------------
;	Update the PR2(PWM Preiod) and CCP1L (Duty Cycle) registers with PR2Value and SineValue
	call 	UpdatePR2andDuty

;------------------------------------------------------------------------------------------------------------
	banksel	T2CON
	bsf	T2CON,TMR2ON		; Start Timer2
	incf	SineSamplePosition,F	; read the next Sine point in the SineTable
	incf	SymphonyPosition,F	; read the next musical value, when the current note duration has
					; been achieved
	clrf	Timer1_Overflows	; Reset the Overflow counter
	incf	Timer1_Overflows,F
	call	Config_Timer1		; Reconfigure Timer1 for 125mS interrupt

End_Timer1:	
	return

;------------------------------------------------------------------------------------------------------------
;*******************Timer2_ISR*******************Timer2_ISR*******************Timer2_ISR*********************
;------------------------------------------------------------------------------------------------------------
;	Description:   Timer2 interrupt is used to read all the values in the correct SineTable and continues
;			to read these values until a Timer1 overflow interrupt has occurred.
;
;	No. of Instructions:	18
;	Stack Depth :		4
;	Flags Used :		C
;------------------------------------------------------------------------------------------------------------
Timer2_ISR:
	bcf	PIR1,TMR2IF		; Clear the Timer2 flag bit

;------------------------------------------------------------------------------------------------------------
;	Check if the Sampling point is between 0-31 or 32-63
;	0-31 corresponds to a point between 0-180 degrees of the Sine wave
;	32-63 correspond to a point between 180-360 degrees of the Sine wave 
	
	movlw	D'32'
	subwf	SineSamplePosition,W
	btfsc	STATUS,C		;between 32-63
	call	NegativeSine		;Calculate the SineValue for the negative part of the Sine Wave
	movlw	D'32'
	subwf	SineSamplePosition,W
	btfss	STATUS,C		;between 0-31
	call	PostiveSine		;Calculate the SineValue for the positive part of the Sine Wave
	
;------------------------------------------------------------------------------------------------------------
	incf	SineSamplePosition,F	; Read the next sine point in the SineTable on the next TMR2 interrupt
	btfsc	SineSamplePosition,6	; check if we have sampled all the sine points
	clrf	SineSamplePosition	; Reached end of SineTable. Start Sampling again from the first
					; sample point 
	call	Divide			; Divide the SineValue (Duty Cycle) by either 2 or 4
	movf	SineValue,W
	banksel	CCPR1L
	movwf	CCPR1L			; Update the Duty Cycle
	return

;------------------------------------------------------------------------------------------------------------
;*******************PostiveSine*******************PostiveSine*******************PostiveSine******************
;------------------------------------------------------------------------------------------------------------
;	Description:	This Subroutine creates the positive half of the Sine wave (0-180) and executes only
;			when the Sample Value is between 0 and 31
;
;	No. of Instructions:	10 
;	Execution Time :	32uS
;	Stack Depth :		3
;	Flags Used :		None
;------------------------------------------------------------------------------------------------------------
PostiveSine:
	movf	SineSamplePosition,W
	movwf	SineTablePoint		; The position of the SinePoint to return from the SineTable
	movf	MusicTableSelect,W	; indicates which SineTable to call
	movlw	HIGH Sine_Tables
	movwf	PCLATH
	call	Sine_Tables		; obtain the correct SineValue
	movlw	HIGH ISR
	movwf	PCLATH
	return

;------------------------------------------------------------------------------------------------------------
;*******************NegativeSine******************NegativeSine******************NegativeSine*****************
;------------------------------------------------------------------------------------------------------------
;	Description:	This Subroutine creates the negative half of the Sine wave (180-360 degrees) and 
;			executes only when the Sample Value is between 32 and 63
;
;	No. of Instructions:	12 
;	Execution Time :	34uS 
;	Stack Depth :		3
;	Flags Used :		None
;------------------------------------------------------------------------------------------------------------
NegativeSine:
	movwf	SineTablePoint		; The position of the SinePoint to return from the SineTable
	movf	MusicTableSelect,W
	movlw	HIGH Sine_Tables
	movwf	PCLATH
	call	Sine_Tables		; Obtain the next SineValue but the SineValue found here is for 
					; the Positive side of the sine wave 
	movlw	HIGH ISR
	movwf	PCLATH
	movf	SineValue,W
	subwf	PR2Value,W		; Calculate the correct SineValue for the Negative part of the
					; sine wave
	movwf	SineValue
	return

;------------------------------------------------------------------------------------------------------------
;****************UpdatePR2andDuty***************UpdatePR2andDuty***************UpdatePR2andDuty**************
;------------------------------------------------------------------------------------------------------------
;	Description: Update the PR2 (PWM Period) and CCP1L (Duty Cycle) registers
;
;	No. of Instructions:	8 
;	Execution Time :	10uS 
;	Stack Depth :		1
;	Flags Used :		None
;------------------------------------------------------------------------------------------------------------
UpdatePR2andDuty:
	movf	SineValue,W
	banksel	CCPR1L
	movwf	CCPR1L
	banksel	PR2
	movf	PR2Value,W
	movwf	PR2
	return

;------------------------------------------------------------------------------------------------------------
;*****************FirstSineValue****************FirstSineValue****************FirstSineValue*****************
;-----------------------------------------------------------------------------------------------------------
;	Description: Find the first Sine sample value in the correct Sine table
;
;	No. of Instructions:	13 
;	Execution Time :	57uS 
;	Stack Depth :		3
;	Flags Used :		C
;------------------------------------------------------------------------------------------------------------
FirstSineValue:		
	call	Sine_Table_Address	; Determine which Sine table to call, and what value must the
					; SineValue be divided by

;	Multiply by 4 to obtain the correct offset to the Sine_Tables subroutine
	bcf	STATUS,C
	rlf	MusicTableSelect,F
	bcf	STATUS,C
	rlf	MusicTableSelect,F
	movlw	HIGH Sine_Tables
	movwf	PCLATH
	call	Sine_Tables		; call the correct SineTable and store the sine sample value to 
					; SineValue
	movlw	HIGH ISR
	movwf	PCLATH
	call	Divide			; Divide the SineValue (Duty Cycle) by either 2 or 4
	return

;------------------------------------------------------------------------------------------------------------
;*******************ReadNewNote*******************ReadNewNote*******************ReadNewNote******************
;------------------------------------------------------------------------------------------------------------
;	Description: Read the next musical value from SymphonyTable
;
;	No. of Instructions:	8 
;	Execution Time :	16uS 
;	Stack Depth :		2
;	Flags Used :		None
;------------------------------------------------------------------------------------------------------------
ReadNewNote:
	movlw	HIGH SymphonyTable	; Obtain the high bits of the SymphonyTable Address
	movwf	PCLATH
	call	SymphonyTable
	movwf	MusicByte		; Store the musical value
	movlw	HIGH ISR		; Make upper bits of PC point to this memory page
	movwf	PCLATH
	return

;------------------------------------------------------------------------------------------------------------
;*******************FindDuration*******************FindDuration*******************FindDuration***************
;------------------------------------------------------------------------------------------------------------ 
;	Description: Find the required music note duration
;
;	No. of Instructions:	9 
;	Execution Time :	11uS 
;	Stack Depth :		2
;	Flags Used :		C
;------------------------------------------------------------------------------------------------------------
FindDuration:
	movf	MusicByte,W
	movwf	NoteDuration
	bcf	STATUS,C
	rrf	NoteDuration,F		; Shift one to the right
	swapf 	NoteDuration,F
	movlw 	D'7'
	andwf 	NoteDuration,F		; Store the required note duration
	return

;------------------------------------------------------------------------------------------------------------
;******************FindMusicNote******************FindMusicNote******************FindMusicNote***************
;------------------------------------------------------------------------------------------------------------
;	Description: Determine which musical note to play 
;	No. of Instructions:	7 
;	Execution Time :	9uS 
;	Stack Depth :		1
;	Flags Used :		None
;------------------------------------------------------------------------------------------------------------
FindMusicNote:
		
	movf	MusicByte,W
	movwf	MusicNote
	bcf	MusicNote,7
	bcf	MusicNote,6
	bcf	MusicNote,5		; Store the musical note
	return

;------------------------------------------------------------------------------------------------------------
;**************Sine_Table_Address**************Sine_Table_Address**************Sine_Table_Address************
;------------------------------------------------------------------------------------------------------------
;	Description:   Find the position of the correct SineTable to call located in the Sine_Tables 
;			subroutine. Also, determine the value that SineValue needs to be divided by.
;
;	No. of Instructions:	22 
;	Execution Time :	14 - 19 uS 
;	Stack Depth :		1
;	Flags Used :		C
;
;	Note: The PR2 value is not divided because I have a table (MusicNotePR2) of all the possible PR2
;	values.
;------------------------------------------------------------------------------------------------------------
 Sine_Table_Address:
;	Determine if the Musical Note ranges between 0-12, 13-24, 25-31

	movlw	D'13'
	subwf	MusicNote,W
	btfss	STATUS,C
	goto	Less13
	movlw	D'25'
	subwf	MusicNote,W
	btfss	STATUS,C
	goto	More13Less25
	goto	More25

;------------------------------------------------------------------------------------------------------------
;	The Music Frequency corresponds to one of the 12 SineTable stored in memory. Therefore, no reason to
;	divide the SineValue (CCPR1L)

Less13:
	clrf	Divider	
	movf	MusicNote,W
	goto	End_SineAddress

;------------------------------------------------------------------------------------------------------------
;	The Music Frequency required to be played corresponds to a frequency two times the stored frequency.
;	Therefore divide all Duty Cycle (SineValue) values by 2. 

More13Less25:
	bsf	Divider,0		; Indicates a divide by two
	movlw	D'12'
	subwf	MusicNote,W		; Obtain the position of the SineTable to call
	goto	End_SineAddress

;------------------------------------------------------------------------------------------------------------
;	The Music Frequency required to be played corresponds to a frequency four times the stored frequency.
;	Therefore divide all Duty Cycle (SineValue) values by 4.

More25:
	bsf	Divider,1		; Indicates a divide by four
	movlw	D'24'
	subwf	MusicNote,W
	;goto	End_SineAddress

;------------------------------------------------------------------------------------------------------------
End_SineAddress
	movwf 	MusicTableSelect	; Store the position of the SineTable to call located in the
					; Sine_Tables subroutine
	return

;------------------------------------------------------------------------------------------------------------
;***********************Divide***********************Divide***********************Divide*********************
;------------------------------------------------------------------------------------------------------------
;	Description: Divide the SineValue by either 2 or 4

;	No. of Instructions:	11 
;	Execution Time :	10 - 13 uS
;	Stack Depth :		1
;	Flags Used :		C
;------------------------------------------------------------------------------------------------------------
Divide:
	bcf	STATUS,C
	btfsc	Divider,0		; Divide by 2 ?
	rrf	SineValue,F
	btfss	Divider,1		; Divide by 4 ?
	goto	EndDivide
	bcf	STATUS,C
	rrf	SineValue,F
	bcf	STATUS,C
	rrf	SineValue,F

EndDivide:
	return

;------------------------------------------------------------------------------------------------------------
;*********************SineTables********************SineTables********************SineTables*****************
;------------------------------------------------------------------------------------------------------------
;	This page contains the following SineTables:
;	NoSine, Sine110Hz, Sine117Hz, Sine124Hz, Sine131Hz, Sine139Hz, Sine147Hz

org 0x102

NoSine:
	movf SineTablePoint,W		;offset to return the correct SineValue
	addwf PCL,F
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'
	retlw D'0'

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
Sine110Hz:
	movf SineTablePoint,W
	addwf PCL,F
	retlw D'71'
	retlw D'78'
	retlw D'85'
	retlw D'92'
	retlw D'98'
	retlw D'105'
	retlw D'111'
	retlw D'116'
	retlw D'122'
	retlw D'126'
	retlw D'130'
	retlw D'134'
	retlw D'137'
	retlw D'139'
	retlw D'140'
	retlw D'141'
	retlw D'141'
	retlw D'140'
	retlw D'139'
	retlw D'137'
	retlw D'134'
	retlw D'130'
	retlw D'126'
	retlw D'122'
	retlw D'116'
	retlw D'111'
	retlw D'105'
	retlw D'98'
	retlw D'92'
	retlw D'85'
	retlw D'78'
	retlw D'71'

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
Sine117Hz:
	movf SineTablePoint,W
	addwf PCL,F
	retlw D'67'
	retlw D'73'
	retlw D'80'
	retlw D'86'
	retlw D'93'
	retlw D'99'
	retlw D'104'
	retlw D'110'
	retlw D'115'
	retlw D'119'
	retlw D'123'
	retlw D'126'
	retlw D'129'
	retlw D'131'
	retlw D'132'
	retlw D'133'
	retlw D'133'
	retlw D'132'
	retlw D'131'
	retlw D'129'
	retlw D'126'
	retlw D'123'
	retlw D'119'
	retlw D'115'
	retlw D'110'
	retlw D'104'
	retlw D'99'
	retlw D'93'
	retlw D'86'
	retlw D'80'
	retlw D'73'
	retlw D'67'

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
Sine124Hz:
	movf SineTablePoint,W
	addwf PCL,F
	retlw D'63'
	retlw D'69'
	retlw D'75'
	retlw D'81'
	retlw D'87'
	retlw D'93'
	retlw D'98'
	retlw D'103'
	retlw D'108'
	retlw D'112'
	retlw D'116'
	retlw D'119'
	retlw D'121'
	retlw D'123'
	retlw D'124'
	retlw D'125'
	retlw D'125'
	retlw D'124'
	retlw D'123'
	retlw D'121'
	retlw D'119'
	retlw D'116'
	retlw D'112'
	retlw D'108'
	retlw D'103'
	retlw D'98'
	retlw D'93'
	retlw D'87'
	retlw D'81'
	retlw D'75'
	retlw D'69'
	retlw D'63'

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
Sine131Hz:
	movf SineTablePoint,W
	addwf PCL,F
	retlw D'59'
	retlw D'65'
	retlw D'71'
	retlw D'77'
	retlw D'82'
	retlw D'88'
	retlw D'93'
	retlw D'97'
	retlw D'102'
	retlw D'106'
	retlw D'109'
	retlw D'112'
	retlw D'114'
	retlw D'116'
	retlw D'117'
	retlw D'118'
	retlw D'118'
	retlw D'117'
	retlw D'116'
	retlw D'114'
	retlw D'112'
	retlw D'109'
	retlw D'106'
	retlw D'102'
	retlw D'97'
	retlw D'93'
	retlw D'88'
	retlw D'82'
	retlw D'77'
	retlw D'71'
	retlw D'65'
	retlw D'59'

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
Sine139Hz:
	movf SineTablePoint,W
	addwf PCL,F
	retlw D'56'
	retlw D'61'
	retlw D'67'
	retlw D'72'
	retlw D'77'
	retlw D'82'
	retlw D'87'
	retlw D'92'
	retlw D'96'
	retlw D'99'
	retlw D'103'
	retlw D'105'
	retlw D'108'
	retlw D'109'
	retlw D'110'
	retlw D'111'
	retlw D'111'
	retlw D'110'
	retlw D'109'
	retlw D'108'
	retlw D'105'
	retlw D'103'
	retlw D'99'
	retlw D'96'
	retlw D'92'
	retlw D'87'
	retlw D'82'
	retlw D'77'
	retlw D'72'
	retlw D'67'
	retlw D'61'
	retlw D'56'

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
Sine147Hz:
	movf SineTablePoint,W
	addwf PCL,F
	retlw D'53'
	retlw D'58'
	retlw D'63'
	retlw D'68'
	retlw D'73'
	retlw D'78'
	retlw D'82'
	retlw D'87'
	retlw D'91'
	retlw D'94'
	retlw D'97'
	retlw D'100'
	retlw D'102'
	retlw D'103'
	retlw D'104'
	retlw D'105'
	retlw D'105'
	retlw D'104'
	retlw D'103'
	retlw D'102'
	retlw D'100'
	retlw D'97'
	retlw D'94'
	retlw D'91'
	retlw D'87'
	retlw D'82'
	retlw D'78'
	retlw D'73'
	retlw D'68'
	retlw D'63'
	retlw D'58'
	retlw D'53'

;------------------------------------------------------------------------------------------------------------
;************************Tables***********************Tables**********************Tables*********************
;------------------------------------------------------------------------------------------------------------
;	This page contains the following SineTables:
;	Sine156Hz,Sine165Hz,Sine175Hz,Sine185Hz,Sine196Hz,Sine208Hz,Sine220Hz

;	MusicNotePR2==> contains 32 different PR2 values. Used to generate the correct PWM frequency required
;	for a specific sine frequency

org 0x202
Sine156Hz:
	movf SineTablePoint,W
	addwf PCL,F
	retlw D'50'
	retlw D'55'
	retlw D'59'
	retlw D'64'
	retlw D'69'
	retlw D'74'
	retlw D'78'
	retlw D'82'
	retlw D'85'
	retlw D'89'
	retlw D'92'
	retlw D'94'
	retlw D'96'
	retlw D'97'
	retlw D'98'
	retlw D'99'
	retlw D'99'
	retlw D'98'
	retlw D'97'
	retlw D'96'
	retlw D'94'
	retlw D'92'
	retlw D'89'
	retlw D'85'
	retlw D'82'
	retlw D'78'
	retlw D'74'
	retlw D'69'
	retlw D'64'
	retlw D'59'
	retlw D'55'
	retlw D'50'

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
Sine165Hz:
	movf SineTablePoint,W
	addwf PCL,F
	retlw D'47'
	retlw D'52'
	retlw D'56'
	retlw D'61'
	retlw D'66'
	retlw D'70'
	retlw D'74'
	retlw D'78'
	retlw D'81'
	retlw D'84'
	retlw D'87'
	retlw D'89'
	retlw D'91'
	retlw D'92'
	retlw D'93'
	retlw D'94'
	retlw D'94'
	retlw D'93'
	retlw D'92'
	retlw D'91'
	retlw D'89'
	retlw D'87'
	retlw D'84'
	retlw D'81'
	retlw D'78'
	retlw D'74'
	retlw D'70'
	retlw D'66'
	retlw D'61'
	retlw D'56'
	retlw D'52'
	retlw D'47'

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
Sine175Hz:
	movf SineTablePoint,W
	addwf PCL,F
	retlw D'44'
	retlw D'48'
	retlw D'53'
	retlw D'57'
	retlw D'61'
	retlw D'65'
	retlw D'69'
	retlw D'73'
	retlw D'76'
	retlw D'79'
	retlw D'81'
	retlw D'84'
	retlw D'85'
	retlw D'87'
	retlw D'87'
	retlw D'88'
	retlw D'88'
	retlw D'87'
	retlw D'87'
	retlw D'85'
	retlw D'84'
	retlw D'81'
	retlw D'79'
	retlw D'76'
	retlw D'73'
	retlw D'69'
	retlw D'65'
	retlw D'61'
	retlw D'57'
	retlw D'53'
	retlw D'48'
	retlw D'44'

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
Sine185Hz:
	movf SineTablePoint,W
	addwf PCL,F
	retlw D'42'
	retlw D'46'
	retlw D'50'
	retlw D'54'
	retlw D'58'
	retlw D'62'
	retlw D'65'
	retlw D'69'
	retlw D'72'
	retlw D'74'
	retlw D'77'
	retlw D'79'
	retlw D'80'
	retlw D'82'
	retlw D'83'
	retlw D'83'
	retlw D'83'
	retlw D'83'
	retlw D'82'
	retlw D'80'
	retlw D'79'
	retlw D'77'
	retlw D'74'
	retlw D'72'
	retlw D'69'
	retlw D'65'
	retlw D'62'
	retlw D'58'
	retlw D'54'
	retlw D'50'
	retlw D'46'
	retlw D'42'

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
Sine196Hz:
	movf SineTablePoint,W
	addwf PCL,F
	retlw D'40'
	retlw D'43'
	retlw D'47'
	retlw D'51'
	retlw D'55'
	retlw D'59'
	retlw D'62'
	retlw D'65'
	retlw D'68'
	retlw D'71'
	retlw D'73'
	retlw D'75'
	retlw D'77'
	retlw D'78'
	retlw D'79'
	retlw D'79'
	retlw D'79'
	retlw D'79'
	retlw D'78'
	retlw D'77'
	retlw D'75'
	retlw D'73'
	retlw D'71'
	retlw D'68'
	retlw D'65'
	retlw D'62'
	retlw D'59'
	retlw D'55'
	retlw D'51'
	retlw D'47'
	retlw D'43'
	retlw D'40'

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
Sine208Hz:
	movf SineTablePoint,W
	addwf PCL,F
	retlw D'37'
	retlw D'41'
	retlw D'44'
	retlw D'48'
	retlw D'52'
	retlw D'55'
	retlw D'58'
	retlw D'61'
	retlw D'64'
	retlw D'66'
	retlw D'68'
	retlw D'70'
	retlw D'72'
	retlw D'73'
	retlw D'74'
	retlw D'74'
	retlw D'74'
	retlw D'74'
	retlw D'73'
	retlw D'72'
	retlw D'70'
	retlw D'68'
	retlw D'66'
	retlw D'64'
	retlw D'61'
	retlw D'58'
	retlw D'55'
	retlw D'52'
	retlw D'48'
	retlw D'44'
	retlw D'41'
	retlw D'37'

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
MusicNotePR2:
	movf MusicNote,W
	addwf PCL,F
	retlw D'0'		;no sound
	retlw D'141'
	retlw D'133'
	retlw D'126'
	retlw D'118'
	retlw D'112'
	retlw D'105'
	retlw D'99'
	retlw D'94'
	retlw D'88'
	retlw D'83'
	retlw D'79'
	retlw D'74'
	retlw D'71'
	retlw D'67'
	retlw D'63'
	retlw D'59'
	retlw D'56'
	retlw D'53'
	retlw D'50'
	retlw D'47'
	retlw D'44'
	retlw D'42'
	retlw D'40'
	retlw D'37'
	retlw D'35'
	retlw D'33'
	retlw D'32'
	retlw D'30'
	retlw D'28'
	retlw D'26'
	retlw D'25'

;------------------------------------------------------------------------------------------------------------
;***********************Tables***********************Tables***********************Tables*********************
;------------------------------------------------------------------------------------------------------------
;	Sine_Tables==> Calls the correct SineTable and returns with the SinePoint in the W register, which is
;	thereafter transferred to the SineValue Register

;	SymphonyTable==> Contains a tables of notes that follow a specific format and are used to generate a 
;	musical tone/Symphony.

org 0x302

Sine_Tables:
	movf	MusicTableSelect,W	;offset to call the correct Sine table
	addwf	PCL,F
	
	movlw	High NoSine
	movwf	PCLATH
	call	NoSine
	goto	EndFineSine

	movlw	High Sine110Hz
	movwf	PCLATH
	call	Sine110Hz
	goto	EndFineSine

	movlw	High Sine117Hz
	movwf	PCLATH
	call	Sine117Hz
	goto	EndFineSine

	movlw	High Sine124Hz
	movwf	PCLATH
	call	Sine124Hz
	goto	EndFineSine

	movlw	High Sine131Hz
	movwf	PCLATH
	call	Sine131Hz
	goto	EndFineSine

	movlw	High Sine139Hz
	movwf	PCLATH
	call	Sine139Hz
	goto	EndFineSine

	movlw	High Sine147Hz
	movwf	PCLATH
	call	Sine147Hz
	goto	EndFineSine

	movlw	High Sine156Hz
	movwf	PCLATH
	call	Sine156Hz
	goto	EndFineSine

	movlw	High Sine165Hz
	movwf	PCLATH
	call	Sine165Hz
	goto	EndFineSine

	movlw	High Sine175Hz
	movwf	PCLATH
	call	Sine175Hz
	goto	EndFineSine

	movlw	High Sine185Hz
	movwf	PCLATH
	call	Sine185Hz
	goto	EndFineSine

	movlw	High Sine196Hz
	movwf	PCLATH
	call	Sine196Hz
	goto	EndFineSine

	movlw	High Sine208Hz
	movwf	PCLATH
	call	Sine208Hz

EndFineSine:
	movwf	SineValue
	movlw	High ISR
	movwf	PCLATH
	return

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
SymphonyTable:
	movf	SymphonyPosition,W
	addwf	PCL,F
	retlw	0x68
	retlw	0x2B 
	retlw	0x8B 
	retlw	0x68 
	retlw	0x26 
	retlw	0x84 
	retlw	0x46 
	retlw	0x48 
	retlw	0x6B 
	retlw	0x28 
	retlw	0xE6 
	retlw	0x68 
	retlw	0x2B 
	retlw	0x8B 
	retlw	0x68 
	retlw	0x26 
	retlw	0x84 
	retlw	0x46 
	retlw	0x48 
	retlw	0x66 
	retlw	0x24 
	retlw	0xC4 
	retlw	0x00		;my termination value

;------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------
end
