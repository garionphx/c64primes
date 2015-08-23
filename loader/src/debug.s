
.code
            sei
            jsr install
            bcs error

            ldx #<filename
            ldy #>filename
            jsr loaduncompd

error:      sta $0400

            sta $d021
            inc $d020
            jmp *-3

filename:   .byte "15*", $00
