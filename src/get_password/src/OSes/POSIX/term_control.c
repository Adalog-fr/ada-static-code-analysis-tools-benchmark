/*
 * MIT License
 * 
 * Copyright (c) 2021 My Ada library
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/*
 * This file defines the two functions used to turn on and off the
 * "password mode"
 */

#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <stdlib.h>

struct termios oldattr;

struct termios* disable_echo()
{
  struct termios newattr, *oldattr;

  /* see man termios for details */
  
  oldattr = malloc (sizeof (*oldattr));

  tcgetattr (STDIN_FILENO, oldattr);
  newattr = *oldattr;
  newattr.c_lflag &= ~(ICANON | ECHO | ECHONL | ISIG);
  newattr.c_iflag
    &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);

  tcsetattr (STDIN_FILENO, TCSANOW, &newattr);

  return oldattr;
}

void restore_echo (struct termios *oldattr)
{
  tcsetattr (STDIN_FILENO, TCSANOW, oldattr);

  free(oldattr);
}

int is_stdin_a_tty ()
{
  return isatty(fileno(stdin));
}
