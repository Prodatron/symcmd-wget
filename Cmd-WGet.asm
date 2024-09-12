;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                           SymbOS network daemon                            @
;@                                  W G E T                                   @
;@                                                                            @
;@               (c) 2015 by Prodatron / SymbiosiS (Jörn Mika)                @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;todo
;- ctrl+C
;- continue download
;- alle header zeigen



;### PRGPRZ -> Programm-Prozess
prgprz  call SyShell_PARALL     ;get commandline parameters
        push de
        call SyShell_PARSHL     ;fetch shell-specific parameters
        jp c,prgend
        ld hl,txtmsgtit         ;title text
        call SyShell_STROUT0
        call SyNet_NETINI                   ;*** INIT NETWORK API (search for the daemon)
        ld hl,txterrdmn:jr c,prgend0    ;no daemon found -> error
        pop de
        ld a,d
        cp 1                            ;1 or 2 parameters
        ld hl,txterrpar:jp c,prgend0
        jr z,prgprz1
        cp 2
        jr nz,prgend0
        ld hl,(1*3+0+SyShell_CmdParas)      ;*** GET FILENAME
        ld (filnam),hl
prgprz1 ld de,(0*3+0+SyShell_CmdParas)      ;*** GET URL
        call wgturl
        jr c,prgend0
        call wgtfil                         ;*** CREATE FILEPATH
        call wgtlod                         ;*** DOWNLOAD DATA
        jr c,prgend0
        jr prgend

;### PRGEND -> Programm beenden
prgend0 push hl
        ld hl,txtmsglf2:call SyShell_STROUT0
        pop hl
        call SyShell_STROUT0
prgend  ld e,0
        call SyShell_EXIT       ;tell Shell, that process will quit
        ld hl,(App_BegCode+prgpstnum)
        call SySystem_PRGEND
prgend1 rst #30
        jr prgend1


txtmsgtit   db 13,10
            db "WGET 1.0 for SymbOS (c)oded 2015 by Prodatron",13,10,13,10,0
txtmsgdot   db "... ",0
txtmsglf2   db 13,10
txtmsglfd   db 13,10,0
txtmsg001   db "Resolving ",0
txtmsg002   db "###.###.###.###",0
txtmsg003   db "Connecting to ",0
txtmsg004   db "|:#####",0
txtmsg005   db "connected.",13,10,0
txtmsg006   db "HTTP request send, awaiting response... ",0
txtmsg007   db "Length: ":txtmsg007a db "unknown":ds 3+1
txtmsg008   db " (######K)",0
txtmsg009   db " [":ds 64+2
txtmsg010   db "XXX% [================>] ",0
txtmsg011   db "##########",13,0
txtmsg012   db "' saved",13,10,13,10,0

txterrpar   db "Wrong or missing parameter",13,10
            db "WGET <url> (<filename>)",13,10,0
txterrdmn   db "Network daemon not running!",13,10,0
txterrpro   db "Protocol not supported",13,10,0
txterrurl   db "Invalid URL",13,10,0
txterrprt   db "Invalid port number",13,10,0
txterrfil   db "Error while creating file",13,10,0
txterrdns   db "Host domain look-up failed",13,10,0
txterrsck   db "No free socket",13,10,0
txterrcon   db "Error while connecting to host",13,10,0
txterrrqs   db "Error while sending HTTP request",13,10,0
txterrhed   db "Error while receiving HTTP response header",13,10,0
txterrunh   db "Unknown header type",13,10,0
txterrunr   db "Unsupported HTTP response",13,10,0
txterrrcv   db "Error while receiving data",13,10,0
txterrwrt   db "Error while writing file",13,10,0

txthtprqs1  db "GET /":txthtprqs10
txthtprqs1a db " HTTP/1.1":txthtprqs2a db 13,10:txthtprqs1a0
txthtprqs2  db "Host: ":txthtprqs20
txthtprqs3  db "Connection: close",13,10
            db "User-Agent: SymShellWGET/1.0 (SymbOS 3.0; MSX)",13,10
            db 13,10:txthtprqs30

txthtprsp1  db "HTTP/",0
txthtprsp2  db "Content-Length: ",0
txthtprsp3  db "Content-Type: ",0

wgtpro  db 0    ;protocol (0=HTTP)
wgtadr  dw 0    ;pointer to address
wgtprt  dw 0    ;port
wgtpth  dw 0    ;pointer to path

nethnd  db 0

filhnd  db 0
filnam  dw 0
fildfl  db "index.htm",0
filtmp  ds 8+1+3+1


;### WGTSTA -> plots current download status
wgtsta  ld a,(wgtlodlek)
        or a
        jr z,wgtsta1
        ld iy,(wgtlodlps)
        ld bc,100
        push iy
        call wgtsta0
        ld hl,32*256+32
        ld (txtmsg010+2),hl
        ld hl,txtmsg010
        call clcn08
        ld (hl),"%"
        pop iy
        ld bc,16
        call wgtsta0
        ld hl,txtmsg010+6
        ld c,"="
        ld b,a
        call wgtsta2
        ld (hl),">"
        inc hl
        ld a,16
        sub b
        ld c," "
        call nz,wgtsta3
        ld hl,txtmsg010:call SyShell_STROUT0
wgtsta1 ld ix,(wgtlodrcv+0)
        ld de,(wgtlodrcv+2)
        ld iy,txtmsg011
        call clcn32
        ld (iy+1),13
        ld (iy+2),0
        ld hl,txtmsg011:jp SyShell_STROUT0
wgtsta0 push bc         ;bc=factor(16 or 100) -> A=percentage (0-max)
        ld e,(iy+4+2)
        ld d,(iy+4+3)
        call clcmul
        ld c,l
        ld b,h
        ld e,(iy+2)
        ld d,(iy+3)
        call clcdiv
        pop bc
        ld a,c
        inc h:dec h
        ret nz
        cp l
        ret c
        ld a,l
        ret
wgtsta2 or a            ;progress bar
        ret z
wgtsta3 ld (hl),c
        inc hl
        dec a
        jr nz,wgtsta3
        ret

;### WGTLOD -> loads a file from the URL
wgtlodlin   db 0    ;number of received header lines
wgtlodtyk   db 0    ;flag, if known content type
wgtlodlek   db 0    ;flag, if known total number
wgtlodlen   ds 4    ;total number of bytes
wgtlodrcv   ds 4    ;received number of bytes
wgtlodlps   dw 0    ;length pointer offset for percentage calculation

wgtlodscn   db 50   ;status counter (display once per second)
wgtlodsup   db 0    ;status update flag (display only on update)

wgtlod  ld hl,tmpbuf
        ld a,(App_BnkNum)
        db #dd:ld h,a
        xor a
        call SyFile_FILNEW                      ;** create file
        ld hl,txterrfil:ret c
        ld (filhnd),a

        ld hl,txtmsg001:call SyShell_STROUT0    ;** host domain lookup
        ld hl,(wgtadr) :call SyShell_STROUT0
        ld hl,txtmsgdot:call SyShell_STROUT0
        ld hl,(wgtadr)
        call SyNet_DNSRSV   
        ld hl,txterrdns:jp c,wgtloda
        push ix
        push iy
        ld hl,txtmsg002
        ld e,"."
        db #dd:ld a,l:call clcn08:ld (hl),e:inc hl
        db #dd:ld a,h:call clcn08:ld (hl),e:inc hl
        db #fd:ld a,l:call clcn08:ld (hl),e:inc hl
        db #fd:ld a,h:call clcn08:ld (hl),0
        ld hl,txtmsg002:call SyShell_STROUT0
        ld hl,txtmsglfd:call SyShell_STROUT0

        ld hl,txtmsg003:call SyShell_STROUT0    ;** open TCP connection
        ld hl,(wgtadr) :call SyShell_STROUT0
        ld d,"|"       :call SyShell_CHROUT0
        ld hl,txtmsg002:call SyShell_STROUT0
        ld de,0
        ld ix,(wgtprt)
        ld iy,txtmsg004+2
        call clcn32
        ld hl,txtmsg004:call SyShell_STROUT0
        ld hl,txtmsgdot:call SyShell_STROUT0
        pop iy
        pop ix
        ld hl,-1
        ld de,(wgtprt)
        xor a
        call SyNet_TCPOPN   
        ld hl,txterrsck:jp c,wgtloda
        ld (nethnd),a

wgtlod9 rst #30                                 ;** wait for established
        call SyNet_NETEVT
        jr c,wgtlod9
        ld a,l
        and 127
        cp 2
        jr c,wgtlod9
        ld hl,txterrcon:scf:jp nz,wgtlodb
        ld hl,txtmsg005:call SyShell_STROUT0

        ld de,tmpbuf                            ;** send HTTP header
        ld hl,txthtprqs1        ;GET
        ld bc,txthtprqs10-txthtprqs1
        ldir
        ld hl,(wgtpth)
        call copzer
        ld hl,txthtprqs1a
        ld bc,txthtprqs1a0-txthtprqs1a
        ldir
        call netsnd0
        ld hl,txterrrqs:jp c,wgtlodb
        ld de,tmpbuf
        ld hl,txthtprqs2        ;HOST
        ld bc,txthtprqs20-txthtprqs2
        ldir
        ld hl,(wgtadr)
        call copzer
        ld hl,txthtprqs2a
        ldi:ldi
        call netsnd0
        ld hl,txterrrqs:jp c,wgtlodb
        ld hl,txthtprqs3        ;CONNECTION, USER-AGENT
        ld bc,txthtprqs30-txthtprqs3
        call netsnd
        ld hl,txterrrqs:jp c,wgtlodb
        ld hl,txtmsg006:call SyShell_STROUT0

wgtlod2 rst #30                 ;*** HEADER
        call SyNet_NETEVT           ;check for data received or close
        jr c,wgtlod2
        bit 7,l
        jr nz,wgtlod3
        ld a,l
        and 127
        cp 3
        ccf
        ld hl,txterrhed:jp c,wgtlodb
        jr wgtlod2

wgtlod3 ld de,(App_BnkNum)          ;receive line
        ld hl,tmpbuf
        ld a,(nethnd)
        call SyNet_TCPRLN
        ld hl,txterrhed:jp c,wgtlodb
        jr z,wgtlod2
        ld a,d
        or a
        ld hl,wgtlodlin
        jp z,wgtlod7
        inc (hl)
        ld a,(hl)
        dec a
        jr nz,wgtlodc
        ld hl,tmpbuf                ;analyse first line -> must be "HTTP/x.x ### Phrase"
        ld de,txthtprsp1
        call strcmp
        ex de,hl
        ld hl,txterrunh:scf:jp nz,wgtlodb
        ld hl,4
        add hl,de
        push hl
        call SyShell_STROUT0
        ld hl,txtmsglfd:call SyShell_STROUT0
        pop de
        db #dd:ld l," "
        call clcr16
        ld a,l
        inc h:dec h
        ld hl,txterrunr:jp c,wgtlodb
        scf:jp nz,wgtlodb
        cp 200
        scf:jp nz,wgtlodb
        jr wgtlod3
wgtlodc ld hl,tmpbuf                ;analyse other lines ->
        ld de,txthtprsp2            ;search for "Content-Length"
        call strcmp
        jr nz,wgtlodd
        push hl
        pop iy
        push hl
        call clcr32
        pop de
        jr c,wgtlod3
        ld a,1
        ld (wgtlodlek),a
        ld (wgtlodlen+0),ix
        ld (wgtlodlen+2),hl
        push ix
        push hl
        ld iy,wgtlodlen
        inc h:dec h
        jr nz,wgtlodj
        dec iy
        inc l:dec l
        jr nz,wgtlodj
        dec iy
wgtlodj ld (wgtlodlps),iy
        ex de,hl
        ld de,txtmsg007a
        call copzer
        ex de,hl
        ld (hl),0
        pop de
        pop hl
        ld l,h
        ld h,e
        ld e,d
        ld d,0
        srl e:rr h:rr l
        srl e:rr h:rr l
        ld bc,1
        add hl,bc
        jr nc,wgtlodg
        inc de
wgtlodg push hl:pop ix
        ld iy,txtmsg008+2
        call clcn32
        ld (iy+1),"K"
        ld (iy+2),")"
        ld (iy+3),0
        jp wgtlod3
wgtlodd ld hl,tmpbuf                ;search for "Content-Type"
        ld de,txthtprsp3
        call strcmp
        jp nz,wgtlod3
        ld a,1
        ld (wgtlodtyk),a
        ld de,txtmsg009+2
        ld bc,256*64+255
wgtlode ld a,(hl)
        or a
        jr z,wgtlodf
        cp ";"
        jr z,wgtlodf
        ldi
        djnz wgtlode
wgtlodf ex de,hl
        ld (hl),"]"
        inc hl
        ld (hl),0
        jp wgtlod3

wgtlod7 ld a,(hl)
        or a
        ld hl,txterrhed:scf:jp z,wgtlodb
        ld hl,txtmsg007:call    SyShell_STROUT0
        ld a,(wgtlodlek)
        or a
        ld hl,txtmsg008:call nz,SyShell_STROUT0
        ld a,(wgtlodtyk)
        or a
        ld hl,txtmsg009:call nz,SyShell_STROUT0
        ld hl,txtmsglf2:call    SyShell_STROUT0

        call wgtsta

        ld a,(SyNet_TCPRLN_Length)
        or a
        jr z,wgtlod5
        ld c,a
        ld b,0
        ld hl,SyNet_TCPRLN_Buffer
        ld de,tmpbuf
        push bc
        ldir
        pop bc
        ld de,1
        jr wgtlod8

wgtlod4 rst #30                 ;*** DATA
        ld hl,wgtlodscn
        dec (hl)
        jr nz,wgtlodh
        ld (hl),50
        ld hl,wgtlodsup
        ld a,(hl)
        ld (hl),0
        or a
        call nz,wgtsta
wgtlodh call SyNet_NETEVT           ;check for data received or close
        jr c,wgtlod4
        bit 7,l
        jr nz,wgtlod5
        ld a,l
        and 127
        cp 3
        jr c,wgtlod4
        call wgtsta             ;** close TCP connection and file
        call wgtlodb
        ld hl,txtmsglf2:call SyShell_STROUT0
        ld d,"'"       :call SyShell_CHROUT0
        ld hl,(wgtfilp):call SyShell_STROUT0
        ld hl,txtmsg012:call SyShell_STROUT0
        or a
        ret

wgtlod5 ld de,(App_BnkNum)          ;receive data
        ld hl,tmpbuf
        ld bc,tmplen
        ld a,(nethnd)
        call SyNet_TCPRCV
        ex de,hl
        ld hl,txterrrcv:jr c,wgtlodb
        ld a,1
        ld (wgtlodsup),a
wgtlod8 push de
        ld a,(filhnd)               ;write to file
        ld de,(App_BnkNum)
        ld hl,tmpbuf
        push bc
        call SyFile_FILOUT
        pop bc
        push af
        ld hl,(wgtlodrcv+0)
        add hl,bc
        ld (wgtlodrcv+0),hl
        jr nc,wgtlodi
        ld hl,(wgtlodrcv+2)
        inc hl
        ld (wgtlodrcv+2),hl
wgtlodi pop af
        pop bc
        ld hl,txterrwrt:jr c,wgtlodb
        ld a,c
        or b
        jr nz,wgtlod5
        jp wgtlod4

wgtlodb push hl             ;abort -> close connection
        ld a,(nethnd)
        call SyNet_TCPDIS
        ld a,(nethnd)
        call SyNet_TCPCLO
        pop hl
wgtloda push hl             ;abort -> close file
        ld a,(filhnd)
        call SyFile_FILCLO
        pop hl
        scf
        ret

;### WGTFIL -> create filepath
;### Input      (filnam)=0 or pointer to filename, wgtpth=pointer to path or to 0
;### Output     tmpbuf=path, (wgtfilp)=pointer to choosen filename
wgtfilp dw 0

wgtfil  ld hl,(filnam)
        inc h:dec h
        jr nz,wgtfil3       ;filename has been specified -> ignore URL and use this one
        ld de,(wgtpth)
        ld a,(de)
        or a
        ld hl,fildfl
        jr z,wgtfil3        ;no path -> use default filename
wgtfil1 ld l,e
        ld h,d
wgtfil2 ld a,(de)           ;search for file in path
        or a
        jr z,wgtfil4
        inc de
        cp "/"
        jr nz,wgtfil2
        ld a,(de)
        or a
        jr nz,wgtfil1
        ld hl,fildfl        ;url ends with path -> use default filename
wgtfil3 ld (wgtfilp),hl
        ld de,0
        ld bc,tmpbuf
        jp SyShell_PTHADD   ;add selected filename with current path
wgtfil4 ld de,filtmp        ;use file in URL for filename
        ld bc,8*256+255
        call wgtfil5
        or a
        jr z,wgtfil8
        ld b,3
        call wgtfil5
        dec de
        xor a
        ld (de),a
wgtfil8 ld hl,filtmp
        jr wgtfil3
wgtfil5 ld a,(hl)           ;copy filepart (8 or 3 chars max)
        ldi
        or a
        ret z
        cp "."
        ret z
        dec b
        jr nz,wgtfil5
wgtfil6 ld a,(hl)
        or a
        jr z,wgtfil7
        cp "."
        jr z,wgtfil7
        inc hl
        jr wgtfil6
wgtfil7 ldi
        ret

;### WGTURL -> Extracts protocol, address, port and path from an URL
;### Input      DE=url
;### Output     wgtpro,wgtadr,wgtprt,wgtpth set, CF=1 invalid url (HL=error message)
wgturlemp   db 0
wgturlpnm   equ 1
wgturlptb   db "http:":ds 3

wgturl  ld b,wgturlpnm      ;*** get protocol
        ld hl,wgturlptb
wgturl1 push bc
        push de
        push hl
wgturl2 ld a,(de)
        call clclcs
        cp (hl)
        jr nz,wgturl3
        inc hl
        ld a,(hl)
        or a
        jr z,wgturl4
        inc de
        jr wgturl2
wgturl3 pop hl
        ld bc,8
        add hl,bc
        pop de
        pop bc
        djnz wgturl1
        ld hl,txterrpro:scf:ret
wgturl4 pop bc
        pop bc
        pop bc
        ld a,wgturlpnm
        sub b
        ld (wgtpro),a
        ex de,hl            ;*** get address
wgturl8 inc hl
        ld a,(hl)
        cp "/"
        jr z,wgturl8
        ld (wgtadr),hl
        ld c,0
wgturl5 ld a,(hl)
        cp ":"
        jr z,wgturl6
        cp "/"
        ld de,80
        jr z,wgturl7
        or a
        jr z,wgturl7
        inc hl
        inc c
        jr wgturl5
wgturl6 ld (hl),0           ;*** get port
        inc hl
        ex de,hl
        db #dd:ld l,"/"
        push bc
        call clcr16
        pop bc
        ex de,hl
wgturl7 ld (wgtprt),de
        ex de,hl
        ld hl,txterrprt:ret c
        ex de,hl            ;*** get path
        ld a,(hl)
        or a
        ld (hl),0
        inc hl
        jr nz,wgturl9
        ld hl,wgturlemp
wgturl9 ld (wgtpth),hl
        inc c
        dec c
        ret nz
        scf
        ld hl,txterrurl:ret


;==============================================================================
;### SUB-ROUTINEN #############################################################
;==============================================================================

;### NETSND -> sends data to a TCP connection
;### Input      A=handle, HL=address, E=bank, BC=length
;### Output     CF=0 ok, CF=1 connection closed (BC=remaining length)
;### Destroyed  ??
netsnd0 ex de,hl
        ld de,tmpbuf
        or a
        sbc hl,de
        ld c,l
        ld b,h
        ex de,hl
netsnd  ld a,(nethnd)
        ld de,(App_BnkNum)
netsnd1 push de
        push hl
        call SyNet_TCPSND   ;-> BC=bytes sent, HL=bytes remaining
        jr c,netsnd2
        jr z,netsnd2
        ex de,hl
        pop hl
        add hl,bc
        ld c,e
        ld b,d
        pop de
        jr netsnd1
netsnd2 pop de
        pop de
        ret

;### COPZER -> copies data until 0-terminator
;### Input      HL=source, DE=destination
copzer  xor a
copzer1 cp (hl)
        ret z
        ldi
        jr copzer1

;### CLCMUL -> Multipliziert zwei Werte (24bit)
;### Eingabe    BC=Wert1 (möglichst kleinerer), DE=Wert2 (möglichst größerer)
;### Ausgabe    A,HL=Wert1*Wert2 (24bit)
;### Veraendert F,BC,DE,IX
clcmul  ld ix,0
        ld hl,0
clcmul1 ld a,c
        or b
        jr z,clcmul3
        srl b
        rr c
        jr nc,clcmul2
        add ix,de
        ld a,h
        adc l
        ld h,a
clcmul2 sla e
        rl d
        rl l
        jr clcmul1
clcmul3 ld a,h
        db #dd:ld e,l
        db #dd:ld d,h
        ex de,hl
        ret

;### CLCDIV -> Division (24bit)
;### Input      A,BC=value1, DE=value2
;### Output     HL=value1/value2, DE=value1 MOD value2
;### Destroyed  AF,BC,DE,IX,IYL
clcdiv  db #dd:ld l,e
        db #dd:ld h,d   ;IX=Wert2(Nenner)
        ld e,a          ;E,BC=Wert1(Zaehler)
        ld hl,0
        db #dd:ld a,l
        or d
        ret z
        ld d,l          ;D,HL=RechenVar
        db #fd:ld l,24  ;IYL=Counter
clcdiv1 rl c
        rl b
        rl e
        adc hl,hl
        rl d
        ld a,l
        db #dd:sub l
        ld l,a
        ld a,h
        db #dd:sbc h
        ld h,a
        ld a,d
        sbc 0
        ld d,a          ;D,HL=D,HL-IX
        jr nc,clcdiv2
        ld a,l
        db #dd:add l
        ld l,a
        ld a,h
        db #dd:adc h
        ld h,a
        adc d
        sub h
        ld d,a
        scf
clcdiv2 db #fd:dec l
        jr nz,clcdiv1
        ex de,hl        ;DE=Wert1 MOD Wert2
        ld a,c
        rla
        cpl
        ld l,a
        ld a,b
        rla
        cpl
        ld h,a          ;HL=Wert1 DIV Wert2
        ret

;### CLCR16 -> Converts string into 16bit value
;### Input      DE=string, IXL=alternative terminator
;### Output     DE=string at terminator (0 or B), HL=value, CF=1 -> invalid format
;### Destroyed  AF,BC,DE
clcr16  ld hl,0
clcr161 ld a,(de)
        or a
        ret z
        db #dd:cp l
        ret z
        sub "0"
        ret c
        cp 10
        ccf
        ret c
        add hl,hl:ret c
        ld c,l
        ld b,h
        add hl,hl:ret c
        add hl,hl:ret c
        add hl,bc:ret c
        add l
        ld l,a
        ld a,0
        adc h
        ret c
        ld h,a
        inc de
        jr clcr161

;### CLCR32 -> Converts ASCII-String (teminated by 0) into 32Bit-number (unsigned)
;### Input      IY=string
;### Output     HL,IX=number, CF=1 overflow
clcr32  ld ix,0
        ld hl,0
clcr321 ld a,(iy+0)
        or a
        ret z
        add ix,ix:adc hl,hl:ret c
        db #dd:ld c,l
        db #dd:ld b,h
        ld e,l
        ld d,h
        add ix,ix:adc hl,hl:ret c
        add ix,ix:adc hl,hl:ret c
        add ix,bc:adc hl,de:ret c   ;HL,IX*=10
        sub "0"
        ld c,a
        ld b,0
        add ix,bc
        ld c,b
        adc hl,bc:ret c             ;HL,IX+=digit
        inc iy
        jr clcr321

;### CLCN08 -> Converts 8bit value into ASCII string (not terminated)
;### Input      A=Value, HL=Destination
;### Output     HL=points behind last digit
;### Destroyed  AF,BC
clcn08  cp 10
        jr c,clcn082
        cp 100
        jr c,clcn081
        ld c,100
        call clcn083
clcn081 ld c,10
        call clcn083
clcn082 add "0"
        ld (hl),a
        inc hl
        ret
clcn083 ld b,"0"-1
clcn084 sub c
        inc b
        jr nc,clcn084
        add c
        ld (hl),b
        inc hl
        ret

;### CLCN32 -> Convert 32Bit number into ASCII string (0-terminated)
;### Input      DE,IX=value, IY=address
;### Output     IY=address of last char
;### Veraendert AF,BC,DE,HL,IX,IY
clcn32t dw 1,0,     10,0,     100,0,     1000,0,     10000,0
        dw #86a0,1, #4240,#f, #9680,#98, #e100,#5f5, #ca00,#3b9a
clcn32z ds 4

clcn32  ld (clcn32z),ix
        ld (clcn32z+2),de
        ld ix,clcn32t+36
        ld b,9
        ld c,0
clcn321 ld a,"0"
        or a
clcn322 ld e,(ix+0):ld d,(ix+1):ld hl,(clcn32z):  sbc hl,de:ld (clcn32z),hl
        ld e,(ix+2):ld d,(ix+3):ld hl,(clcn32z+2):sbc hl,de:ld (clcn32z+2),hl
        jr c,clcn325
        inc c
        inc a
        jr clcn322
clcn325 ld e,(ix+0):ld d,(ix+1):ld hl,(clcn32z):  add hl,de:ld (clcn32z),hl
        ld e,(ix+2):ld d,(ix+3):ld hl,(clcn32z+2):adc hl,de:ld (clcn32z+2),hl
        ld de,-4
        add ix,de
        inc c
        dec c
        jr z,clcn323
        ld (iy+0),a
        inc iy
clcn323 djnz clcn321
        ld a,(clcn32z)
        add "0"
        ld (iy+0),a
        ld (iy+1),0
        ret

;### CLCLCS -> Converts to lowercase
;### Input      A=char
;### Output     A=lcase(char)
;### Destroyed  F
clclcs  cp "A"
        ret c
        cp "Z"+1
        ret nc
        add "a"-"A"
        ret

;### STRCMP -> Compares strings
;### Input      HL=string1, DE=string2 (0-terminated)
;### Output     ZF=1 -> strings are equal
;### Destroyed  AF,DE,HL
strcmp  ld a,(de)
        or a
        ret z
        cp (hl)
        ret nz
        inc hl
        inc de
        jr strcmp


;==============================================================================
;### DATEN-TEIL ###############################################################
;==============================================================================

tmpbuf  db 0    ;###last label!###

App_BegData

;### nothing (more) here
db 0

;==============================================================================
;### TRANSFER-TEIL ############################################################
;==============================================================================

App_BegTrns
;### PRGPRZS -> Stack für Programm-Prozess
            ds 64
prgstk      ds 6*2
            dw prgprz
App_PrcID   db 0
App_MsgBuf  ds 14
