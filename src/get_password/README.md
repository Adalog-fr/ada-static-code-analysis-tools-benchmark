# What is this?

This is a small Ada library that provides a procedure `Get_Password` that reads a string from the terminal replacing each character with a `*`.

# How do I install it?

TO BE WRITTEN (it will use alire)

# Any system dependence?

This works, for sure, with Linux (since I have Linux). I think it should work nicely with other \*nix flavors, but I cannot check it (if you try on another \*nix, let me know). Most probably it can work in Windows with cygwin, but I am not sure.

If you try this on another systems and have info about compatibility and/or patches for another systems, I will be happy to hear it.  See [PORTING.md](PORTING.md) for info about portability.

# How do I use it?

The package provides few versions of the basic procedure (see `get_password.ads` for more details

```ada
  type End_Condition is (Buffer_Full, End_Of_Line, Both);

  procedure Get (Buffer      : out String;
                 Last        : out Natural;
                 Marker      : String := "*";
                 End_On      : End_Condition := Both);
```

This procedure reads a string from the terminal echoing for every character the string `Marker`.  The read string is stored in `Buffer` and `Last` contains the index of the last character.  The procedure can return on two conditions
1. The buffer is filled, that is, the user types `Buffer'Length` characters
2. The user ends the line with the *Return* key

By default both events can end the input, but this can be changed by specifying `End_On`. If `End_On_Line` is specified and the user types more than `Buffer'Length` characters, the excess characters are lost.

Finally, the user can *cancel* the input with the `Esc` key. In this case the empty string is returned (`Last = Buffer'First-1`)

Of course, all this makes sense only if the standard input is a _terminal_, that is, not a file nor a pipe. If the standard input is not a terminal, the procedure `Get` raises `Not_a_TTy_Error`. To avoid the exception, it can be checked if the standard input is a terminal using the function 

```ada
  function Is_A_Tty return Boolean; 
```

If a finer control is desidered, the "extended" version can be used

```ada
   type Error_Code is (Success, Cancelled, Not_A_Tty);
   
   procedure Get (Buffer      : out String;
                  Last        : out Natural;
                  Err_Code    : out Error_Code;
                  Marker      : String := "*";
                  End_On      : End_Condition := Both;
                  Allow_Abort : Boolean := True);
``` 

This version returns in `Err_Code` a value that describes if the reading was successfull or not
* If everything is OK, `Success` is returned
* If the user presses `Esc` and `Allow_Abort` is `True`, then `Cancelled`
* If the standard input is not a terminal, `Not_a_TTY` is returned

The package provides also two `Get` procedures for `Unbounded_String`


## Line editing

Very basic line editing terminal-like capabilities are provided
* `Backspace` or `Del` deletes the last character entered
* `Ctrl-U` delete everything typed so far
* `Esc` aborts the input

# Examples 

See `test/src/main.adb` for an example of usage

