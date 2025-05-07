# Portabilitiy

As said in the README.md, at the moment this library works certainly with Linux and at 99.99% with any other POSIX system.

If you want to port it to another system, here it is how the code is organized

* Under the folder `src/` you find all the source files. In the top-level (that is, right under `src/`) you find the sources that do not depend on the specific system
* Under the folder `src/OSes` you find OS-specific folders.  Currently only `src/OSes/POSIX` is present
* Under each OS-specific folder you find the *body*  of `Get_Password.Terminal_Control` tuned to the specific OS. 

The goal of package `Get_Password.Terminal_Control` is to hide the details of how the terminal is put in *password mode* and back under an OS-independent interface.

## What if I want to port the library to a new OS?

It is fairly simple

1. Create a suitable folder under `src/OSes`
2. Write the version of the body of package `Get_Password.Terminal_Control` specific for the new OS. 
3. Most probably you'll need an OS-specific version of `Get_Password.Terminal_Modes`
   
