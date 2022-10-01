!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!
SYSSIZE = 0x3000
!
!	bootsect.s		(C) 1991 Linus Torvalds
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps ther
!bootsect.s由bios加载到0x7c00处，并将自己移动到0x90000处，并跳转到0x90000处
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!然后bootsect.s将setup直接加载到自己的后面0x90200处，将system加载到0x10000处，这是通过BIOS中断完成的
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.

.globl begtext, begdata, begbss, endtext, enddata, endbss  !定义了全局的，文本开始段，数据开始段，未初始化数据开始段，文本结束段，数据结束段，未初始化数据结束段
.text
begtext:
.data
begdata:
.bss
begbss:
.text

SETUPLEN = 4				! nr of setup-sectors  setup程序的扇区数值
BOOTSEG  = 0x07c0			! original address of boot-sector bootsect的起始地址，为段地址
INITSEG  = 0x9000			! we move boot here - out of the way 将bootsect移动到的起始地址，也为段地址
SETUPSEG = 0x9020			! setup starts here setup.s的程序从这里开始
SYSSEG   = 0x1000			! system loaded at 0x10000 (65536). system模块加载到0x10000处
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading 停止加载的段地址

! ROOT_DEV:	0x000 - same type of floppy as boot. 根文件系统设备使用与引导时同样的软驱设备
!		0x301 - first partition on first drive etc 根文件系统设备在第一个硬盘的第一个分区上
ROOT_DEV = 0x306  !指定根文件系统设备是第2个硬盘的第一个分区。这是Linux老式的硬盘命名方式，具体值含义如下：
										 !设备号 = 主设备号*256 + 次设备号（即  dev_no = (major << 8) + minor）（主设备号：1-内存，2-磁盘，3-硬盘，4-ttyx，5-tty，6-并行寇，7-非命名管道）
										 ! 0x300 - /dev/hd0 - 代表整个第一个硬盘
										 ! 0x301 - /dev/hd1 - 代表第一个盘的第一个分区
										 ! 0x306 - /dev/hd6 - 代表第二个盘的第一个分区

entry _start 				!告知连接程序，程序从start标号开始执行
_start:                                       !52-62段代码将bootsect.s从目前段位置0x07c0（31k）处移动到0x9000（576k）处，共256个字（512个字节），然后跳转到移动后代码的go标号处
	mov	ax,#BOOTSEG          
	mov	ds,ax							!将ds段寄存器置为0x7c00，不能直接将数据写入数据段寄存器ds中，而是先将数据写入到通用寄存器ax，在将通用寄存器ax写入到数据段寄存器ds中
	mov	ax,#INITSEG
	mov	es,ax							!将es段寄存器置为0x9000，ES 附加段寄存器、专门配合数据内存地址的寄存器、它与DI变指寄存器配合使用。
													!通常DS与SI取得数据的源地址、ES与DI取得数据的目标地址
													!功能：ES：数据段地址存储
													!功能：DI：数据段偏移地址存储
	mov	cx,#256						 !移动计数值 = 256
	sub	si,si
	sub	di,di
	rep											!重复执行movw操作，重复次数为256次
	movw								   !一次移动一个字，从ds：si （源地址）移动到 es:di（目标地址）
	jmpi	go,INITSEG			 !间接跳转。这里INITSEG指出跳转到的段地址，从下面开始CPU执行已移动到0x9000段处的代码了
												   !CS和IP是8086 CPU中2个最关键的寄存器。它们指示了要读取指令的地址。大部分8086 CPU寄存器的值，
												   !都可以使用mov指令来改变，但CS、IP中的值不能用mov指令来修改。能够改变CS、IP寄存器内容的指令称为转移指令
go:	mov	ax,cs						!
	mov	ds,ax
	mov	es,ax							
! put stack at 0x9ff00.
	mov	ss,ax						   !通过通用寄存器ax，将cs=0x9000赋值给通用寄存器ax，在将通用寄存器的值ax赋值给数据段寄存器ds和扩展寄存器es和堆栈段寄存器ss
												  !栈段寄存器SS，存放段地址，SP寄存器存放偏移地址，任意时刻，SS:SP指向栈顶元素
	mov	sp,#0xFF00		! arbitrary value >>512  设置栈段内偏移  sp寄存器存放偏移地址

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.    !es已经被设置好了，es已经指向目的段地址处0x9000

load_setup:							!load_setup的用途是利用BIOS中断INT0x13将setup模块从磁盘第2个扇区，开始读到0x90200开始处，共读4个扇区。如果读出错，则复位驱动器
												 !并重试，没有退路。
												 !INT 0x13的使用方法如下：
												 !ax寄存器中有  ah和al   ah：0x02读磁盘扇区到内存，al：需要读出的扇区数量
												 !cx寄存器中有	ch和cl    ch：磁道号的低8位，                cl：开始扇区（0-5位），磁道号高2位（6-7）；
												 !dx寄存器中有 dh和dl   dh：磁头号                                    dl：驱动器号
												 !es:bx : 指向数据缓冲区;  如果出错则CF标志置位
	mov	dx,#0x0000		! drive 0, head 0
	mov	cx,#0x0002		! sector 2, track 0
	mov	bx,#0x0200		! address = 512, in INITSEG  INITSEG = 0x9000
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors
											
	int	0x13			! read it 读取磁盘到内存，读4个扇区，从0驱动号，0磁头号，0磁道号，开始扇区为2，读进es：bx指向的数据缓冲区，如果出现错误则CF标志置位
	jnc	ok_load_setup		! ok - continue
												!故名思意，如果成功就跳到下面
                              					!ok_load_setup处，否则执行下面的代码，
                              					!复位磁盘再次执行这段代码
	mov	dx,#0x0000			!再次设置驱动器为软盘驱动器
	mov	ax,#0x0000		! reset the diskette
											!入口参数为 ah=00h 重启磁盘(软盘系
                              				!统复位），这个时候由于前面的操作磁
                              				!盘已经转到某个位置，所以必须重新启动
                              				!磁盘，让磁头归位.
	int	0x13					   !将参数重新设置好之后重新启用13号中断
	j	load_setup			  !跳转到load_setup继续执行

ok_load_setup:

! Get disk drive parameters, specifically nr of sectors/track
!获取磁盘驱动器的参数，特别是每道的扇区数量
!int 0x13中断的输入参数
!ah = 0x08  bl =  驱动器号
!返回信息：
!如果出错则CF标志置位，并且ah = 状态码
!ah = 0,al = 0,    bl = 驱动器类型
!ch = 最大磁道号的低8位，cl = 每磁道最大扇区数（位0-5），最大磁道号高2位
!dh = 最大磁头数， dl = 驱动器数量
!es：di 软驱磁盘参数表

	mov	dl,#0x00            !表示获取软驱磁盘参数
	mov	ax,#0x0800		! AH=8 is get drive parameters
	int	0x13						!添加中断13，读取参数
	mov	ch,#0x00			!因为要获取的是磁道扇区数，所以高8为置为0x00
	seg cs							!表示下一条语句的操作数在cs段寄存器所指的段中
	mov	sectors,cx		!保存每磁道扇区数
	mov	ax,#INITSEG
	mov	es,ax

! Print some inane message  显示一些信息

	mov	ah,#0x03		! read cursor pos 
	xor	bh,bh
	int	0x10
	
	mov	cx,#24
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov	bp,#msg1
	mov	ax,#0x1301		! write string, move cursor
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000)

	mov	ax,#SYSSEG
	mov	es,ax		! segment of 0x010000
	call	read_it
	call	kill_motor

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.

	seg cs
	mov	ax,root_dev
	cmp	ax,#0
	jne	root_defined
	seg cs
	mov	bx,sectors
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
undef_root:
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:

	jmpi	0,SETUPSEG

! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
sread:	.word 1+SETUPLEN	! sectors read of current track  当前磁道已经读取的扇区数，前面的1表示引导扇区bootsect.s 
head:	.word 0			! current head 当前磁头数
track:	.word 0			! current track	当前磁道数

read_it:
	mov ax,es
	test ax,#0x0fff
die:	jne die			! es must be at 64kB boundary
	xor bx,bx		! bx is starting address within segment
rp_read:
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?
	jb ok1_read
	ret
ok1_read:
	seg cs
	mov ax,sectors
	sub ax,sread
	mov cx,ax
	shl cx,#9
	add cx,bx
	jnc ok2_read
	je ok2_read
	xor ax,ax
	sub ax,bx
	shr ax,#9
ok2_read:
	call read_track
	mov cx,ax
	add ax,sread
	seg cs
	cmp ax,sectors
	jne ok3_read
	mov ax,#1
	sub ax,head
	jne ok4_read
	inc track
ok4_read:
	mov head,ax
	xor ax,ax
ok3_read:
	mov sread,ax
	shl cx,#9
	add bx,cx
	jnc rp_read
	mov ax,es
	add ax,#0x1000
	mov es,ax
	xor bx,bx
	jmp rp_read

read_track:
	push ax
	push bx
	push cx
	push dx
	mov dx,track
	mov cx,sread
	inc cx
	mov ch,dl
	mov dx,head
	mov dh,dl
	mov dl,#0
	and dx,#0x0100
	mov ah,#2
	int 0x13
	jc bad_rt
	pop dx
	pop cx
	pop bx
	pop ax
	ret
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

!/*
! * This procedure turns off the floppy drive motor, so
! * that we enter the kernel in a known state, and
! * don't have to worry about it later.
! */
kill_motor:
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb
	pop dx
	ret

sectors:
	.word 0

msg1:
	.byte 13,10
	.ascii "yzt is booting ..."
	.byte 13,10,13,10

.org 508
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55

.text
endtext:
.data
enddata:
.bss
endbss:
