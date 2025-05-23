## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S T A R T                                 #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                      Copyright (C) 2012-2021 AdaCore                      #
#                                                                           #
#  GNAT is free software;  you can  redistribute it  and/or modify it under #
#  terms of the  GNU General Public License as published  by the Free Soft- #
#  ware  Foundation;  either version 3,  or (at your option) any later ver- #
#  sion.  GNAT is distributed in the hope that it will be useful, but WITH- #
#  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY #
#  or FITNESS FOR A PARTICULAR PURPOSE.                                     #
#                                                                           #
#  As a special exception under Section 7 of GPL version 3, you are granted #
#  additional permissions described in the GCC Runtime Library Exception,   #
#  version 3.1, as published by the Free Software Foundation.               #
#                                                                           #
#  You should have received a copy of the GNU General Public License and    #
#  a copy of the GCC Runtime Library Exception along with this program;     #
#  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    #
#  <http://www.gnu.org/licenses/>.                                          #
#                                                                           #
#############################################################################

	.syntax unified
	.cpu cortex-m0
	.thumb

	/* This is the vector table used by all RP2040 runtimes */
	.text
	.section .vectors,"a"
	.globl __vectors
	.p2align 8
__vectors:
	/* Cortex-M core interrupts */
	.word   __stack_end          /* stack top address */
	.word   _start_rom           /* 1 Reset.  */
	.word   fault                /* 2 NMI. */
	.word   fault                /* 3 Hard fault. */
	.word   fault                /* 4 reserved. */
	.word   fault                /* 5 reserved. */
	.word   fault                /* 6 reserved. */
	.word   fault                /* 7 reserved. */
	.word   fault                /* 8 reserved. */
	.word   fault                /* 9 reserved. */
	.word   fault                /* 10 reserved. */
	.word   __gnat_sv_call_trap  /* 11 SVCall. */
	.word   fault                /* 12 reserved. */
	.word   fault                /* 13 reserved. */
	.word   __gnat_pend_sv_trap  /* 14 PendSV. */
	.word   __gnat_sys_tick_trap /* 15 Systick. */
	/* MCU interrupts */
	.word __gnat_irq_trap        /* 16 TIMER_IRQ_0 */
	.word __gnat_irq_trap        /* 17 TIMER_IRQ_1 */
	.word __gnat_irq_trap        /* 18 TIMER_IRQ_2 */
	.word __gnat_irq_trap        /* 19 TIMER_IRQ_3 */
	.word __gnat_irq_trap        /* 20 PWM_IRQ_WRAP */
	.word __gnat_irq_trap        /* 21 USBCTRL */
	.word __gnat_irq_trap        /* 22 XIP */
	.word __gnat_irq_trap        /* 23 PIO0_IRQ_0 */
	.word __gnat_irq_trap        /* 24 PIO0_IRQ_1 */
	.word __gnat_irq_trap        /* 25 PIO1_IRQ_0 */
	.word __gnat_irq_trap        /* 26 PIO1_IRQ_1 */
	.word __gnat_irq_trap        /* 27 DMA_IRQ_0 */
	.word __gnat_irq_trap        /* 28 DMA_IRQ_1 */
	.word __gnat_irq_trap        /* 29 IO_IRQ_BANK0 */
	.word __gnat_irq_trap        /* 30 IO_IRQ_QSPI */
	.word __gnat_irq_trap        /* 31 SIO_IRQ_PROC0 */
	.word __gnat_irq_trap        /* 32 SIO_IRQ_PROC1 */
	.word __gnat_irq_trap        /* 33 CLOCKS */
	.word __gnat_irq_trap        /* 34 SPI0 */
	.word __gnat_irq_trap        /* 35 SPI1 */
	.word __gnat_irq_trap        /* 36 UART0 */
	.word __gnat_irq_trap        /* 37 UART1 */
	.word __gnat_irq_trap        /* 38 ADC_IRQ_FIFO */
	.word __gnat_irq_trap        /* 39 I2C0 */
	.word __gnat_irq_trap        /* 40 I2C1 */
	.word __gnat_irq_trap        /* 41 RTC */
	.word __gnat_irq_trap        /* 42 (unused) */
	.word __gnat_irq_trap        /* 43 (unused) */
	.word __gnat_irq_trap        /* 44 (unused) */
	.word __gnat_irq_trap        /* 45 (unused) */
	.word __gnat_irq_trap        /* 46 (unused) */
	.word __gnat_irq_trap        /* 47 (unused) */

	.text
	.thumb_func
	.globl _start_rom
_start_rom:
	/* Check which core we're running on */
    ldr r0, =0xd0000000 /* CPUID register */
    ldr r0, [r0]

	/* Call _wait_for_vector in the bootrom for all cores except core0 */
    cmp r0, #0
    bne jump_to_bootrom_wait_for_vector

	/* Copy .data */
	ldr	r0,=__data_start
	ldr	r1,=__data_words
	ldr	r2,=__data_load
        cmp	r1,#0
	beq	1f
0:	ldr	r4,[r2]
	str	r4,[r0]
        adds	r0,#4
        adds	r2,#4
	subs	r1,r1,#1
	bne	0b
1:
	/* Clear .bss */
	ldr	r0,=__bss_start
	ldr	r1,=__bss_words
	movs	r2,#0
        cmp	r1,#0
	beq	1f
0:	str	r2,[r0]
        adds	r0,#4
	subs	r1,r1,#1
	bne	0b

1:
	bl __gnat_initialize_bootrom
	bl	_ada_setup_clocks
	bl	main
	bl	_exit

hang:   b .

jump_to_bootrom_wait_for_vector:
	/* Lookup and call _wait_for_vector, see 2.8.3.1.5 in RP2040 datasheet */
    ldr r0, = 'W' | ('V' << 8) 
    bl __gnat_rom_func_lookup
    bx r0

	.text

	.thumb_func
.weak __gnat_poke_handler
.type __gnat_poke_handler, %function
__gnat_poke_handler:
0:	b 0b
	.size __gnat_poke_handler, . - __gnat_poke_handler

	.thumb_func
.weak __gnat_irq_trap
.type __gnat_irq_trap, %function
__gnat_irq_trap:
0:	b 0b
	.size __gnat_irq_trap, . - __gnat_irq_trap

	.thumb_func
.weak __gnat_sv_call_trap
.type __gnat_sv_call_trap, %function
__gnat_sv_call_trap:
0:	b 0b
	.size __gnat_sv_call_trap, . - __gnat_sv_call_trap

	.thumb_func
.weak __gnat_pend_sv_trap
.type __gnat_pend_sv_trap, %function
__gnat_pend_sv_trap:
0:	b 0b
	.size __gnat_pend_sv_trap, . - __gnat_pend_sv_trap

	.thumb_func
.weak __gnat_sys_tick_trap
.type __gnat_sys_tick_trap, %function
__gnat_sys_tick_trap:
0:	b 0b
	.size __gnat_sys_tick_trap, . - __gnat_sys_tick_trap

	.thumb_func
fault:	b fault

/* Hack to force the linker to link boot2.o */
.word __boot2
