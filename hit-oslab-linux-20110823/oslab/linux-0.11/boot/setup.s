!
!	setup.s		(C) 1991 Linus Torvalds
!
! setup.s is responsible for getting the system data from the BIOS,
! and putting them into the appropriate places in system memory.
! both setup.s and system has been loaded by the bootblock.
! setup.s 负责从BIOS中获取系统数据，并将这些数据放到系统内存的适当地方
!此时setup.s和system已经被bootsect代码加载到了内存
! This code asks the bios for memory/disk/other parameters, and
! puts them in a "safe" place: 0x90000-0x901FF, ie where the
! boot-block used to be. It is then up to the protected mode
! system to read them from there before the area is overwritten
! for buffer-blocks.
!这段代码询问bios有关内存/磁盘/其他参数，并将这些参数放到一个安全的地方，然后在缓冲块覆盖掉之前由保护模式的system读取

! NOTE! These had better be the same as in bootsect.s!以下参数最好是与bootsect.s中的相同

INITSEG  = 0x9000	! we move boot here - out of the way
SYSSEG   = 0x1000	! system loaded at 0x10000 (65536).
SETUPSEG = 0x9020	! this is the current segment

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

entry start
start:
! ok, the read went well so we get current cursor position and save it for 
! posterity.  整个读磁盘过程都正常，现在将光标位置保存以备今后使用

	mov	ax,#INITSEG	! this is done in bootsect already, but...
	mov	ds,ax               !对ds重新进行赋值
	mov	ah,#0x03	! read cursor pos  使用int 0x10中断读取光标的位置
	xor	bh,bh
	int	0x10		! save it in known place, con_init fetches
	mov	[0],dx		! it from 0x90000.  将1光标的位置保存在0x90000处
! Get memory size (extended mem, kB) !下面三句取拓展内存的大小值
																				 !是调用中断0x15，功能号为ah = 0x88
																				 !返回：ax = 从0x10000处开始的拓展内存的大小
																				 !如果出错则CF置位，ax = 出错码

	mov	ah,#0x88
	int	0x15
	mov	[2],ax !将拓展内存数值保存在0x90002处（1个字）

! Get video-card data: !下面这段用于取显示卡当前显示模型
											 ! 调用BIOS中断0x10，功能号ah = 0x0f
											 !返回：ah = 字符列数  al=显示模式，bh = 当前显示页

	mov	ah,#0x0f
	int	0x10
	mov	[4],bx		! bh = display page
	mov	[6],ax		! al = video mode, ah = window width

! check for EGA/VGA and some config parameters

	mov	ah,#0x12
	mov	bl,#0x10
	int	0x10
	mov	[8],ax
	mov	[10],bx
	mov	[12],cx

! Get hd0 data  !取第一个硬盘的信息（复制硬盘参数表）
							   !第1个硬盘参数表的首地址是中断向量0x41的向量值,而第2个硬盘参数表紧接着第一个表的后面，中断向量0x46向量值也指向这第二个硬盘的参数表首地址

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x41]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0080
	mov	cx,#0x10
	rep
	movsb

! Get hd1 data

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x46]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	rep
	movsb

! Check that there IS a hd1 :-)

	mov	ax,#0x01500
	mov	dl,#0x81
	int	0x13
	jc	no_disk1
	cmp	ah,#3
	je	is_disk1
no_disk1:
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	mov	ax,#0x00
	rep
	stosb
is_disk1:

! now we want to move to protected mode ...  现在我们要进入保护模式了

	cli			! no interrupts allowed !此时不允许中断

! first we move the system to it's rightful place  首先我们将系统移动到正确的地方，bootsect引导程序是将system模块读入到从0x10000（64k）的位置
! 由于当时假设system模块最大长度不会超过0x80000（512k），也即其末端不会超过内存0x90000，所以bootsect会将自己移动到0x90000开始的地方，并将setup加载到它的
!后面，下面这段程序是再把整个system模块移动到0x0000处，即把从0x10000到0x8ffff的内存数据块（512k）,整块地向内存低端移动0x10000（64k）的位置

	mov	ax,#0x0000
	cld			! 'direction'=0, movs moves forward
do_move:
	mov	es,ax		! destination segment   es:ip=0x0000,目的地址（初始为0x0000：0x0）
	add	ax,#0x1000
	cmp	ax,#0x9000
	jz	end_move
	mov	ds,ax		! source segment
	sub	di,di
	sub	si,si
	mov 	cx,#0x8000
	rep
	movsw
	jmp	do_move

! then we load the segment descriptors 此后加载段描述符
! 从这里开始会遇到32位保护模式的操作，因此需要Intel 32位保护模式编程方面的知识
! 在进入保护模式中运行之前，我们需要首先设置好使用的段描述符，这里需要设置全局描述符表和中断描述符表
! 
! lidt 指令用于加载中断描述符表（idt）寄存器，它的操作数是6个字节，0-1字节是描述符表的长度值（字节）；2-5字节是描述符表的32位线性基地址（首地址）
! 其形式参见下面的说明。中断描述符表中的每一个表项（8字节）指出发生中断是需要调用的代码的信息，与中断向量有些相似，但是包含更多的信息。
! lgdt 指令用于加载全局描述符（gdt）寄存器，全局描述符中的每个描述符项（8字节）描述了保护模式下数据和代码段（块）的信息。其中包括段的最大长度
! 限制（16位），段的线性基址（32位），段的特权级，段是否在内存，读写许可以及其他一些保护模式运行的标志。

end_move:
	mov	ax,#SETUPSEG	! right, forgot this at first. didn't work :-)
	mov	ds,ax
	lidt	idt_48		! load idt with 0,0  加载中断描述符表（idt）寄存器，idt_48是6字节操作数的位置。前2字节表示idt表的限长，后4字节表示idt表所处的基地址
	lgdt	gdt_48		! load gdt with whatever appropriate 

! that was painless, now we enable A20  现在开启A20地址线

	call	empty_8042
	mov	al,#0xD1		! command write
	out	#0x64,al
	call	empty_8042
	mov	al,#0xDF		! A20 on
	out	#0x60,al
	call	empty_8042

! well, that went ok, I hope. Now we have to reprogram the interrupts :-( 
! we put them right after the intel-reserved hardware interrupts, at
! int 0x20-0x2F. There they won't mess up anything. Sadly IBM really
! messed this up with the original PC, and they haven't been able to
! rectify it afterwards. Thus the bios puts interrupts at 0x08-0x0f,
! which is used for the internal hardware interrupts as well. We just
! have to reprogram the 8259's, and it isn't fun.
! 我们必须重新对中断进行编程，我们将它们放在正好处于intel保留的硬件中断后面在int0x20 - 0x2f
	mov	al,#0x11		! initialization sequence
	out	#0x20,al		! send it to 8259A-1
	.word	0x00eb,0x00eb		! jmp $+2, jmp $+2
	out	#0xA0,al		! and to 8259A-2
	.word	0x00eb,0x00eb
	mov	al,#0x20		! start of hardware int's (0x20)
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x28		! start of hardware int's 2 (0x28)
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x04		! 8259-1 is master
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x02		! 8259-2 is slave
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x01		! 8086 mode for both
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0xFF		! mask off all interrupts for now
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al

! well, that certainly wasn't fun :-(. Hopefully it works, and we don't
! need no steenking BIOS anyway (except for the initial loading :-).
! The BIOS-routine wants lots of unnecessary data, and it's less
! "interesting" anyway. This is how REAL programmers do it.
!
! Well, now's the time to actually move into protected mode. To make
! things as simple as possible, we do no register set-up or anything,
! we let the gnu-compiled 32-bit programs do that. We just jump to
! absolute address 0x00000, in 32-bit protected mode.
! 这里设置进入32位保护模式运行。首先加载机器状态字也称控制器寄存器CR0其比特位0置位1将导致CPU工作在保护模式
	mov	ax,#0x0001	! protected mode (PE) bit
	lmsw	ax		! This is it!
	jmpi	0,8		! jmp offset 0 of segment 8 (cs)

! This routine checks that the keyboard command queue is empty
! No timeout is used - if this hangs there is something wrong with
! the machine, and we probably couldn't proceed anyway.
! 我们已经将system模块移动到0x00000开始的地方，所以这里的偏移地址是0。这里的段值8已经是保护模式下的段选择符了，用于选择描述符表和描述符表项
! 以及所要求的特权级。段选择描述符长度为16位（2个字节），0-1位表示请求的特权级0-3，linux操作系统只用到了两级：0级（核心级）和3级（用户级）；2位用于选择
!全局描述符表0还是局部描述符表1，3-15位是描述符表项的索引，指出选择第几项描述符。所以段选择描述符8（0b0000，0000，0000，1000）表示选择全局描述符中的第一项
! 该项指出代码的基地址是0，因此这里的跳转指令就会执行system代码。
empty_8042:
	.word	0x00eb,0x00eb
	in	al,#0x64	! 8042 status port
	test	al,#2		! is input buffer full?
	jnz	empty_8042	! yes - loop
	ret

gdt:
	.word	0,0,0,0		! dummy

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9A00		! code read/exec
	.word	0x00C0		! granularity=4096, 386

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9200		! data read/write
	.word	0x00C0		! granularity=4096, 386

idt_48:
	.word	0			! idt limit=0
	.word	0,0			! idt base=0L

gdt_48:
	.word	0x800		! gdt limit=2048, 256 GDT entries
	.word	512+gdt,0x9	! gdt base = 0X9xxxx

.text
endtext:
.data
enddata:
.bss
endbss:
