;MAB8048H, 10.000MHz
;Warszawa 2023
	.cr	8048
	.tf	rom.bin,BIN
	.lf	therm.lst
;==================Defines=====================
;Pins
;P1
cathodes    .eq %01111111 ;Display cathodes on P1.0-P1.6
ow_pin      .eq %10000000 ;1-Wire bus at P1.7

;P2
anode_1 .eq %00010000 ;Anode 1 at P2.4
anode_2 .eq %00100000 ;Anode 2 at P2.5
anode_3 .eq %01000000 ;Anode 3 at P2.6
anode_4	.eq %10000000 ;Anode 4 at P2.7
anodes  .eq %11110000 ;Anodes mask

;Constants
;Time-related
timer_init_val      .eq 152 ;Gives ~200 interrupts per second @10MHz
counter_ovf_val     .eq 100 ;R3 counts to this value to get 500ms delay

;DS18B20
ds18b20_convert_t		.eq $44
ds18b20_read_scratchpad	.eq $BE
ds18b20_skip_rom		.eq $CC

;RAM variables
display_1_buf   .eq $20
display_2_buf   .eq $21
display_3_buf   .eq $22
display_4_buf   .eq $23

;Fixed purpose registers/flags
;F0 - 1-Wire error flag, set when zero (to simplify code) 
;F1 - timer interrupt flag, set when zero
;R2 - current display counter
;R3 - interrupts counter, used for 500ms delay
;R4 - current temperature value

;Macros
sub     .ma A,V ;Subtracts V (Rx or immediate value) from A (A = A - V)
        cpl A
        add A,]2
        cpl A
        .em
	
;Vectors
	.no $00 ;Set jump to main at reset vector (00h)
	jmp main

    .no $07 ;Set jump to timer interrupt ISR at timer interrupt vector (07h)
	jmp timer_isr

main:
    clr A
    mov R2,A ;Clear display counter
    mov R3,A ;Clear interrupts counter
    mov R4,A ;Clear temperature value
    clr F1 ;Set interrupt flag
    call ds18b20_get_temp ;Get new measurement
    call display_update ;Update display buffers
    call timer_load ;Load timer
    en tcnti ;Enable timer interrupt
    strt T ;Start timer
loop:
    jf1 loop ;Wait for interrupt
    cpl F1 ;Clear interrupt flag
    call display_refresh ;Update display
    inc R3 ;Increment interrupts counter
    mov A,R3
    >sub A,#counter_ovf_val ;Check if counter overflown
    jnz skip_refresh ;If not, skip refresh
    mov R3,#0 ;Clear interrupt counter
    call ds18b20_get_temp ;Get new measurement
    call display_update ;Update display buffers
skip_refresh:
	jmp loop
	
display_C       .eq %00111001 ;C
display_E       .eq %01111001 ;E
display_r       .eq %01010000 ;r
display_minus   .eq %01000000 ;-
display_blank   .eq %00000000 ;blank

display_lut:
    .ot ;Open table to verify at assemble time whether LUT and last movp are on the same page
    .db %00111111 ;0
	.db %00000110 ;1
	.db %01011011 ;2
	.db %01001111 ;3
	.db %01100110 ;4
	.db %01101101 ;5
	.db %01111101 ;6
	.db %00000111 ;7
	.db %01111111 ;8
	.db %01101111 ;9

display_update:
    jf0 display_no_error ;Check if 1-Wire bus error
    mov R0,#display_1_buf
    mov @R0,#display_blank
    inc R0
    mov @R0,#display_E
    inc R0
    mov @R0,#display_r
    inc R0
    mov @R0,#display_r ;If error, display 'Err'
    jmp display_update_end
display_no_error:
    mov R0,#display_4_buf
    mov @R0,#display_C ;Load 'C' sign to fourth display buffer
    mov A,R4
    jb7 display_negative ;If MSB set, value is negative
    mov R0,A
    call byte_split ;Split temperature to tens and ones
    mov A,#display_lut
    add A,R0 ;Get display LUT offset for ones digit
    movp A,@A ;Get display code for ones digit
    mov R0,#display_3_buf
    mov @R0,A ;Load display code to third display buffer
    mov A,R1
    jz display_blank_both ;If tens are zero, blank two first displays
    mov R0,A ;Otherwise load tens from R1 to R0
    call byte_split ;Split tens to hundreds and tens
    mov A,#display_lut
    add A,R0 ;Get display LUT offset for tens digit
    movp A,@A ;Get display code for tens digit
    mov R0,#display_2_buf
    mov @R0,A ;Load display code to second display buffer
    mov A,R1 ;Load hundreds value to A
    jz display_blank_first ;If the digit is zero, do not show it
    mov A,#display_lut
    add A,R1 ;Get display LUT offset for hundreds digit
    movp A,@A ;Get display code for hundreds digit
    mov R0,#display_1_buf
    mov @R0,A ;Load display code to first display buffer
    jmp display_update_end
display_blank_both:
    mov R0,#display_2_buf
    mov @R0,#display_blank ;Blank second display
display_blank_first:
    mov R0,#display_1_buf
    mov @R0,#display_blank ;Blank first display
    jmp display_update_end
display_negative:
    cpl A
    inc A ;A = -A
    mov R0,A
    call byte_split ;Split temperature to tens and ones 
    mov A,#display_lut
    add A,R0 ;Get display LUT offset for ones digit
    movp A,@A ;Get display code for ones digit
    mov R0,#display_3_buf
    mov @R0,A ;Load display code to third display buffer
    mov A,R1 ;Load tens value to A
    jz display_no_tens ;If the digit is zero, show minus instead of digit
    add A,#display_lut ;Get display LUT offset for tens digit
    movp A,@A ;Get display code for tens digit
    .ct ;Close table
    mov R0,#display_2_buf
    mov @R0,A ;Load display code to second display buffer
    mov R0,#display_1_buf
    mov @R0,#display_minus ;Load '-' sign to first display buffer
    jmp display_update_end
display_no_tens:
    mov R0,#display_2_buf
    mov @R0,#display_minus ;Load '-' sign to second display buffer
    mov R0,#display_1_buf
    mov @R0,#display_blank ;Blank first display
display_update_end:
    ret

display_refresh:
    orl P2,#anodes ;Turn off all anodes
    anl P1,#~cathodes ;Turn off all cathodes
    mov A,R2 ;switch(R2)
    jz display_refresh_1 ;case 0
    cpl A
    inc A ;A = -R2
    inc A
    jz display_refresh_2 ;case 1
    inc A
    jz display_refresh_3 ;case 2
    inc A
    jz display_refresh_4 ;case 3
    jmp display_refresh_reset ;default
display_refresh_1:
    in A,P1 ;Get current P1 state
    mov R0,#display_1_buf
    orl A,@R0 ;Load first display character from RAM to A (preserving MSB state)
    outl P1,A ;Write A to port
    anl P2,#~anode_1 ;Turn on first display 
    inc R2 ;Select next display
    jmp display_refresh_end
display_refresh_2:
    in A,P1 ;Get current P1 state
    mov R0,#display_2_buf
    orl A,@R0 ;Load second display character from RAM to A
    outl P1,A ;Write A to port
    anl P2,#~anode_2 ;Turn on second display 
    inc R2 ;Select next display
    jmp display_refresh_end
display_refresh_3:
    in A,P1 ;Get current P1 state
    mov R0,#display_3_buf
    orl A,@R0 ;Load third display character from RAM to A
    outl P1,A ;Write A to port
    anl P2,#~anode_3 ;Turn on third display 
    inc R2 ;Select next display
    jmp display_refresh_end
display_refresh_4:
    in A,P1 ;Get current P1 state
    mov R0,#display_4_buf
    orl A,@R0 ;Load fourth display character from RAM to A
    outl P1,A ;Write A to port
    anl P2,#~anode_4 ;Turn on fourth display 
display_refresh_reset:
    mov R2,#0
display_refresh_end:
    ret

;Uses R0,R1,R4,R7
ds18b20_get_temp:
	call ow_reset ;Send bus reset condition
	mov R0,#ds18b20_skip_rom 
	call ow_write_byte ;Send skip ROM command
	mov R0,#ds18b20_read_scratchpad 
	call ow_write_byte ;Send read scratchpad command
	call ow_read_byte ;Read temperature LSB
    mov A,R0
    rr A
    rr A
    rr A
    rr A
    anl A,#%00001111 ;Compute LSB >> 4 - discard fractional part
    mov R4,A ;Store computed value in R4
    call ow_read_byte ;Read temperature MSB
    mov A,R0
    rl A
    rl A
    rl A
    rl A
    anl A,#%11110000 ;Compute MSB << 4
    orl A,R4 ;Merge two nibbles into integer temperature value
    mov R4,A ;Store temperature value into R4
	call ow_reset ;Send bus reset condition
	mov R0,#ds18b20_skip_rom ;Skip ROM
	call ow_write_byte
	mov R0,#ds18b20_convert_t ;Convert temp - prepare to next readout
	call ow_write_byte
	ret	

;Uses F0 as error flag, R7
ow_reset:
    clr F0 ;Set 1-Wire error flag
	anl P1,#~ow_pin ;Clear 1-Wire pin
	call delay_500us ;Hold low for 500us
	orl P1,#ow_pin ;Set 1-Wire pin
    call delay_100us ;Wait for 100us for sensor response
    in A,P1
    anl A,#ow_pin ;Probe the line
    jnz ow_reset_error ;If high, sensor did not respond
    cpl F0 ;Clear 1-Wire error flag
ow_reset_error:
	call delay_500us ;Wait for 500us for timeslot to end
	ret

;R0 - received byte, uses R0,R1,R7
ow_read_byte:
	mov R0,#0 ;Clear result
	mov R1,#8 ;Load bit loop counter
ow_read_loop:
	mov R7,#10 ;Load delay loop counter; ~3us
	;Shift result one bit right
	mov A,R0 ;~1.5us
	rr A ;~1.5us
	mov R0,A ;~1.5us	
	;Request read - 1-Wire pin >1us low
	anl P1,#~ow_pin ;Clear 1-Wire pin; ~3us
	orl P1,#ow_pin ;Set 1-Wire pin; ~3us
	;Read bit and complete 60us timeslot
	in A,P1 ;Read P1; ~3us
	anl A,#ow_pin ;Read 1-Wire pin; ~3us
	jz ow_read_zero ;~3us
ow_read_one:
	mov A,R0 ;~1.5us
	orl A,#%10000000 ;~3us
	mov R0,A ;Set bit in result; ~1.5us
ow_read_zero:
	djnz R7,ow_read_zero ;Wait for ~30us; ~3us	
	djnz R1,ow_read_loop ;Receive next bit; ~3us
	ret

;R0 - byte to be written, uses R0,R1,R7	
ow_write_byte:
	mov A,R0 ;Load byte to A
	cpl A ;Because of 8049 limitations - there's no jnbx instruction...
	mov R1,#8 ;Load bit loop counter
ow_write_loop:
	mov R7,#16 ;Load delay loop counter; ~3us
	anl P1,#~ow_pin ;Clear 1-Wire pin; ~3us
	jb0 ow_write_zero ;Check LSB, if not set - send zero; ~3us
ow_write_one:
	orl P1,#ow_pin ;Set 1-Wire pin; ~3us
ow_write_zero:
	djnz R7,ow_write_zero ;Wait for ~50us	
	orl P1,#ow_pin ;Set 1-Wire pin; ~3us
	rr A ;Shift byte one bit right; ~1.5us
	djnz R1,ow_write_loop ;Write next bit; ~3us
	ret

;~100uS delay, uses and corrupts R7
delay_100us:
	mov R7,#29
delay_100us_loop:
	djnz R7,delay_100us_loop
	ret

;~500uS delay, uses and corrupts R7
delay_500us:
	mov R7,#164
delay_500us_loop:
	djnz R7,delay_500us_loop
	ret

;R0 - value to be split to digits, ones; R1 - tens; uses R0,R1
byte_split:
	mov R1,#0 ;Clear tens
byte_split_div10:
	mov A,R0 ;Load value to be split to A
	cpl A ;Complement A
	add A,#10 ;Add 10 (C = (R0 < 10))
	jc byte_split_end ;If there has been carry - break
	cpl A ;Complement A (A=R0-10)
	mov R0,A ;Store new value in R0
	inc R1 ;Increment tens
	jmp byte_split_div10 ;Perform again, until R0 < 10	
byte_split_end:
	ret

timer_load:
    mov A,#timer_init_val
    mov T,A
    ret

timer_isr:
    sel RB1 ;Select second register bank
    mov R0,A ;Preserve A state
    call timer_load ;Reload timer
    clr F1 ;Set interrupt flag
    mov A,R0 ;Restore A state
    retr
