                # load x y z into F G H
top:            CALL getnumber
                JF A end
                SET F A
                CALL getnumber
                SET G A
                CALL getnumber
                SET H A

                # surface area added to D
                MULT A F G
                MULT B G H
                ADD A A B
                MULT B F H
                ADD A A B
                MULT A A 2
                ADD D D A

                # volume added to E
                MULT A F G
                MULT A A H
                ADD E E A

                GT A F G
                JF A skip_swap_1
                SET A F
                SET F G
                SET G A

skip_swap_1:    GT A G H
                JF A skip_swap_2
                SET A G
                SET G H
                SET H A

skip_swap_2:    MULT A F G
                ADD D D A

                ADD A F G
                ADD E E A
                ADD E E A

                JMP top

end:            SET A D
                CALL printnumber
                OUT '\n'
                SET A E
                CALL printnumber
                OUT '\n'
                HALT

getnumber:              PUSH B
                        PUSH C

                        SET A 0
getnumber_loop:         IN B

                        #too small
                        GT C '0' B
                        JT C getnumber_done

                        ADD B B -48 # -'0'

                        #too big
                        GT C 10 B
                        JF C getnumber_done

                        MULT A A 10
                        ADD A A B
                        JMP getnumber_loop

getnumber_done:         POP C
                        POP B
                        RET

### printnumber ########################################
# Print a number in signed, decimal notation
#
# Arguments: A, number to print
# Results:   None
printnumber:            PUSH A # Number to print
                        PUSH B # Temporary condition
                        PUSH C # Output character

                        GT B A 0x3fff # test for negative
                        JF B printnumber_positive

                        OUT '-'
                        MULT A A -1


printnumber_positive:   GT B    10 A
                        JT B printnumber_1
                        GT B   100 A
                        JT B printnumber_10
                        GT B  1000 A
                        JT B printnumber_100
                        GT B 10000 A
                        JT B printnumber_1000

printnumber_10000:      SET C '0'
printnumber_10000_loop: GT B 10000 A
                        JT B printnumber_10000_done
                        ADD A A -10000
                        ADD C C 1
                        JMP printnumber_10000_loop
printnumber_10000_done: OUT C

printnumber_1000:       SET C '0'
printnumber_1000_loop:  GT B 1000 A
                        JT B printnumber_1000_done
                        ADD A A -1000
                        ADD C C 1
                        JMP printnumber_1000_loop
printnumber_1000_done:  OUT C

printnumber_100:        SET C '0'
printnumber_100_loop:   GT B 100 A
                        JT B printnumber_100_done
                        ADD A A -100
                        ADD C C 1
                        JMP printnumber_100_loop
printnumber_100_done:   OUT C

printnumber_10:         SET C '0'
printnumber_10_loop:    GT B 10 A
                        JT B printnumber_10_done
                        ADD A A -10
                        ADD C C 1
                        JMP printnumber_10_loop
printnumber_10_done:    OUT C

printnumber_1:          ADD C A '0'
                        OUT C

POP C
POP B
POP A
RET
### END OF printnumber #################################
