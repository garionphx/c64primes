// Prime number finder. First version will seive the primes up to 100.

// A basic loader. Generates:
//      10 SYS (2064)

// Notes:
//
// curr:            stores the current prime we're working with
// curr_index:      curr / 20
// curr_mask_index: curr % 20
//
// index_adder = amount to add to the index when we go to the next number
// mask_adder = amount to add to mask_index when we got to the next number
//
// bit_index = The physical address of the bits. (bits_index - bits = curr_index)


.pc = $0801 "Basic upstart"
:BasicUpstart(start)

// A few vars
.var bits = $0c00
.var end_bits = bits + $C350 // 50000 bytes
.var curr = $19 // some random place in the zero page that seems safe
.var curr_index = $1e
.var curr_mask_index = $21

.var adder_index = $02
.var bit_mask_index = $41
.var bit_index = $43

.var index_adder_low_1 = $57
.var index_adder_low_2 = $58
.var index_adder_low_3 = $59
.var index_adder_low_4 = $5a
.var index_adder_high_1 = $5b
.var index_adder_high_2 = $5c
.var index_adder_high_3 = $5d
.var index_adder_high_4 = $5e

.var mask_adder_1 = $5f
.var mask_adder_2 = $60
.var mask_adder_3 = $61
.var mask_adder_4 = $62

.var BIT7 = $fb
.var BIT3 = $fc

.var calculating_location = $07b0

.var display_msg_work = $e0
.var display_msg_ptr = $e3

.var index_and_mask_to_hex_work_index = $e4 
.var index_and_mask_to_hex_work_hex = $e7
.var index_and_mask_to_hex_work_mask = $eb
.var hex_lo = $ec
.var hex_hi = $ed

.var bit_mask = $87

.pc = $0810 // 2064, Enough room over the BasicStart

start:
    // Prep some vars
    lda #$80
    sta BIT7
    lda #$08
    sta BIT3

    // Turn off the basic rom 
    lda #$36
    sta $01

    // Back up the zero page, so I can reload basic afterwards
    // This is going into the IO area, but all writes go to RAM, no
    // matter how things area configured
    ldx #$ff
copy_zp:
    lda $00,x
    sta $e000,x
    dex
    bne copy_zp

    // Black out the screen
    lda #$00
    sta $d021
    sta $d020

    // clear the screen
    ldx #$00
    lda #$20
clear_screen:
    dex
    sta $0400,X
    sta $0500,X
    sta $0600,X
    sta $0700,X
    bne clear_screen

    // Display the message
    ldy #$17 // Y
    ldx #$0a // X
    
    lda #[clearing_memory_msg_end - clearing_memory_msg] // length
    sta display_msg_work + 2 // Length
    lda #<clearing_memory_msg
    sta display_msg_work
    lda #>clearing_memory_msg
    sta display_msg_work + 1
    
    jsr display_msg

    // Clear some memory
    lda #$FF
    ldx #>end_bits
outer_clear_mem:
    stx clear_mem + 3 // self modding code.
    ldy #$00
clear_mem:
    dey
    sta bits,y // Self modding code.
    bne clear_mem
    
    dex
    cpx #>bits - 1
    bne outer_clear_mem

    // Copy 20 bytes for the bit mask.
    ldx #$00
copy_table:
    lda bit_mask_table, x
    sta bit_mask, x
    inx
    cpx #$14 // compare to 20
    bne copy_table

    // We know that 1 is not prime, so lets turn that bit off now.
    lda #$FE
    sta bits

    // Some initialization
    lda #$03
    sta bit_mask_index
    sta curr_mask_index

    lda #>bits
    sta bit_index + 1

    // Start with 3 as the first number to seive
    lda #$03 
    sta curr
    lda #$00
    sta curr + 1
    sta curr + 2
    sta curr + 3
    sta curr + 4
    sta curr_index 
    sta curr_index + 1
    sta bit_index

    // Display the message
    ldy #$17 // Y
    ldx #$0a // X
    
    lda #[calculating_msg_end - calculating_msg] // length
    sta display_msg_work + 2 // Length
    lda #<calculating_msg
    sta display_msg_work
    lda #>calculating_msg
    sta display_msg_work + 1


break2:
    jsr display_msg

    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop

next_number:
    // Copy the number to the accum.
    lda curr_mask_index

    // Now multiply curr_index and curr_mask_index by 2 to get the adders
    asl // remainder
    cmp #$14        
    bcc mask_adder_skip_carry_1
    sbc #$14

mask_adder_skip_carry_1:
    sta mask_adder_1
    sta mask_adder_3
    sta mask_adder_4
    lda curr_index
    rol
    sta index_adder_low_1    
    sta index_adder_low_3    
    sta index_adder_low_4    
    lda curr_index + 1
    rol
    sta index_adder_high_1  
    sta index_adder_high_3  
    sta index_adder_high_4  
    
    lda mask_adder_1
    asl // remainder
    cmp #$14        
    bcc mask_adder_skip_carry_2
    sbc #$14
mask_adder_skip_carry_2:
    sta mask_adder_2
    lda index_adder_low_1
    rol
    sta index_adder_low_2    
    lda index_adder_high_1
    rol
    sta index_adder_high_2    
    
    // Now we have our adders. Lets start adding.
    lda #$FF

    sta adder_index

    ldy #$00

add_loop:
    // Determine which adder we're using.
    inc adder_index

    // Do the bin version first
    lda adder_index
    and #$03

    // Store this, its for the index_adder
    tax

    // Figure out the mask_index
    // No need for clc here.
    lda mask_adder_1, X
    adc bit_mask_index

    // Are we >= 20
    cmp #$14  
    bcc no_mask_carry  

    // Yes, subtract 20 from this, and carry to the bit_index
    // Note that carry flag is set here. 
    sbc #$14           // Let the carry flag fall through to the adc bit_index after the no_mask_carry.
    // Carry flag will be set here

no_mask_carry:
    sta bit_mask_index

    // We don't want to clc here, since it may fall from the sbc above.
    lda index_adder_low_1, X
    adc bit_index
    sta bit_index
    lda index_adder_high_1, X
    adc bit_index + 1
    sta bit_index + 1

    // Are we done? We already have the byte we want in there, no need to load
    cmp #>[end_bits]
    bcs r_we_done     // if >=, check the lower byte

set_the_bit:

    // We now have the index we need, and an offset to our bit mask. Turn that 
    // bit off.
    ldx bit_mask_index  // WHich of the masks we are using
    lda bit_mask,X      // Load that mask
    and (bit_index),y   // Add it with the data pointed to by bit_index
    sta (bit_index),y   // Store the updated data

    jmp add_loop

r_we_done:
    // Check the lower byte.
    lda bit_index

    cmp #<[end_bits]
    bcs find_next_num  // >=, then goto the next number
    jmp set_the_bit
    
find_next_num:
    // Find the bit/mask index for the curr num. The next prime to sieve.
    clc
    lda #>bits
    adc curr_index + 1
    sta curr_index + 1

    // curr_mask_index has the mask index
    ldx curr_mask_index

next_num_loop:
    inx                         // 2
    inx                         // 2
    cpx #$14                    // 2
    bcc no_mask_carry_2         // 2, 3 on branch

    // No clc needed here
    txa                         // 2
    sbc #$14                    // 2
    tax                         // 2

    inc curr_index
    bne no_mask_carry_2
    inc curr_index + 1

no_mask_carry_2:
    lda bit_mask, X

    // Now check the bit in that location
    ldy #$00
    ora (curr_index), Y

    cmp #$FF
    bne next_num_loop

    // Store the data for the next loop
    stx curr_mask_index
    stx bit_mask_index

    lda curr_index
    sta bit_index
    lda curr_index + 1
    sta bit_index   + 1
    sec
    sbc #>bits
    sta curr_index + 1

    // Are we done?
    lda curr_index
    cmp #$32            // $32 is the quotient of 1000 / 20.
    bcs summit
    jmp next_number
        
break:
summit: 
    // Display the message
    ldy #$17 // Y
    ldx #$0a // X
    
    lda #[summing_msg_end - summing_msg] // length
    sta display_msg_work + 2 // Length
    lda #<summing_msg
    sta display_msg_work
    lda #>summing_msg
    sta display_msg_work + 1

    jsr display_msg   
   
    // Sum things together.
    lda #$07 // 2 and 5 are prime.
    sta curr
    lda #$00
    sta curr + 1
    sta curr + 2
    sta curr + 3
    sta curr + 4

    lda #<bits
    sta curr_index
    lda #>bits
    sta curr_index + 1

    ldx #$01 // Start on the first bit, next it'll advance to the 3rd bit
next_num_loop_2:
    ldy #$00
    inx 
    inx
    
    cpx #$14
    bcc no_mask_carry_3

    // No clc needed here
    txa
    sbc #$14
    tax

    inc curr_index
    bne no_mask_carry_3
    inc curr_index + 1

no_mask_carry_3:
    // Are we done?
    lda curr_index + 1
    cmp #>[end_bits - 1]
    bcc r_we_done_4     // <, if its less than, skip the second test

r_we_done_3:
    lda curr_index

    cmp #<[end_bits]
    bcs end_prg  // < Both of these mean we continue

r_we_done_4:

    lda bit_mask, X
    beq next_num_loop_2 // skip 0's

    // Now check the bit in that location
    ora (curr_index), Y

    cmp #$FF
    bne next_num_loop_2

    stx curr_mask_index

    // We now have the index and mask, convert to a hex value.
    txa

    sta index_and_mask_to_hex_work_mask
    lda curr_index
    sta index_and_mask_to_hex_work_index
    lda curr_index + 1
    sec
    sbc #>bits
    sta index_and_mask_to_hex_work_index + 1

    jsr index_and_mask_to_hex

    clc
    lda index_and_mask_to_hex_work_hex
    adc curr
    sta curr

    lda index_and_mask_to_hex_work_hex + 1
    adc curr + 1
    sta curr + 1

    lda index_and_mask_to_hex_work_hex + 2
    adc curr + 2
    sta curr + 2

    lda index_and_mask_to_hex_work_hex + 3
    adc curr + 3
    sta curr + 3

    lda #$00
    adc curr + 4
    sta curr + 4
    
    // Display something
    jsr display_curr

    ldx curr_mask_index
    jmp next_num_loop_2

end_prg:
    // copy the zp back
    sei

    // Let us see the io page.
    lda #$34
    sta $01

    // Copy the data back to the zp.
    ldx #$02
    copy_zp_3: lda $e000,x
    sta $00,x
    inx
    bne copy_zp_3

    // Renable the io page and basic.
    lda #$37
    sta $01
    cli
    rts

display_curr:
    ldx #$05
    ldy #$FE
display_loop:
    dex
    iny
    iny
    txa
    pha
    tya
    pha
    lda curr, x
    jsr get_hex
    pla
    tay
    pla
    tax
    lda hex_hi
    sta calculating_location, y
    lda hex_lo
    sta calculating_location + 1, y
    cpx #$00
    bne display_loop
    rts

get_hex:
    // Take a byte in A, and stores the chars to display in x and Y
    tay
    and #$f0
    lsr
    lsr
    lsr
    lsr
    tax
    lda print_table,x
    sta hex_hi
    tya
    and #$0f
    tax
    lda print_table,x
    sta hex_lo
    rts

display_ret:
.byte 0, 0 

display_msg:
    lda #$04
    sta display_msg_ptr + 1
    lda #$00
    sta display_msg_ptr 

add_y:
    cpy #$00
    beq done_y
    
    clc
    lda #40
    adc display_msg_ptr
    sta display_msg_ptr
    lda #$00
    adc display_msg_ptr + 1
    sta display_msg_ptr + 1
    dey
    jmp add_y

done_y:
    txa
    clc
    adc display_msg_ptr
    sta display_msg_ptr
    lda #$00
    adc display_msg_ptr + 1
    sta display_msg_ptr + 1
    
    ldy #$00
write_to_screen:
    lda (display_msg_work),Y
    sta (display_msg_ptr),Y
    iny
    cpy display_msg_work + 2
    bne write_to_screen

    rts

index_and_mask_to_hex:
    lda index_and_mask_to_hex_work_index
    sta index_and_mask_to_hex_work_hex
    lda index_and_mask_to_hex_work_index + 1
    sta index_and_mask_to_hex_work_hex + 1

    lda #$00
    sta index_and_mask_to_hex_work_hex + 2
    sta index_and_mask_to_hex_work_hex + 3

    // Multiply by 16
    ldx #$04
multi_loop_2:
    asl index_and_mask_to_hex_work_hex     
    rol index_and_mask_to_hex_work_hex + 1 
    rol index_and_mask_to_hex_work_hex + 2 
    rol index_and_mask_to_hex_work_hex + 3 
    dex
    bne multi_loop_2

    // Times 4
    lda #$00
    sta index_and_mask_to_hex_work_index + 2

    asl index_and_mask_to_hex_work_index
    rol index_and_mask_to_hex_work_index + 1
    rol index_and_mask_to_hex_work_index + 2
    
    asl index_and_mask_to_hex_work_index
    rol index_and_mask_to_hex_work_index + 1
    rol index_and_mask_to_hex_work_index + 2

    // Add in the mask index
    clc
    lda index_and_mask_to_hex_work_mask
    adc index_and_mask_to_hex_work_index
    sta index_and_mask_to_hex_work_index
    lda #$00
    adc index_and_mask_to_hex_work_index + 1
    sta index_and_mask_to_hex_work_index + 1
    lda #$00
    adc index_and_mask_to_hex_work_index + 2
    sta index_and_mask_to_hex_work_index + 2

    // Now add'em
    clc
    lda index_and_mask_to_hex_work_hex
    adc index_and_mask_to_hex_work_index
    sta index_and_mask_to_hex_work_hex

    lda index_and_mask_to_hex_work_hex + 1
    adc index_and_mask_to_hex_work_index + 1
    sta index_and_mask_to_hex_work_hex + 1

    lda index_and_mask_to_hex_work_hex + 2
    adc index_and_mask_to_hex_work_index + 2
    sta index_and_mask_to_hex_work_hex + 2

    lda index_and_mask_to_hex_work_hex + 3     // continue in case we carry?
    adc #$00
    sta index_and_mask_to_hex_work_hex + 3
    rts




clearing_memory_msg:
.text "clearmem   "
clearing_memory_msg_end:
calculating_msg:
.text "calulating:"
calculating_msg_end:
summing_msg:
.text "summing   :"
summing_msg_end:




// I should see if I find space in the zero page for this. 20 bytes
bit_mask_table:
.byte $00,          // 0
      %11111110,    // 1
      $00,          // 2
      %11111101,    // 3
      $00,          // 4
      $00,          // 5
      $00,          // 6
      %11111011,    // 7
      $00,          // 8
      %11110111,    // 9
      $00,          // 10
      %11101111,    // 11
      $00,          // 12
      %11011111,    // 13
      $00,          // 14
      $00,          // 15
      $00,          // 16
      %10111111,    // 17
      $00,          // 18
      %01111111     // 19
                       
print_table:
.byte $30,
      $31,
      $32,
      $33,                   
      $34,
      $35,
      $36,
      $37,
      $38,
      $39,
      $01,
      $02,
      $03,
      $04,
      $05,
      $06,
      $07
