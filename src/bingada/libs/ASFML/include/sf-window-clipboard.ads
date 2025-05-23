--//////////////////////////////////////////////////////////
-- SFML - Simple and Fast Multimedia Library
-- Copyright (C) 2007-2018 Laurent Gomila (laurent@sfml-dev.org)
-- This software is provided 'as-is', without any express or implied warranty.
-- In no event will the authors be held liable for any damages arising from the use of this software.
-- Permission is granted to anyone to use this software for any purpose,
-- including commercial applications, and to alter it and redistribute it freely,
-- subject to the following restrictions:
-- 1. The origin of this software must not be misrepresented;
--    you must not claim that you wrote the original software.
--    If you use this software in a product, an acknowledgment
--    in the product documentation would be appreciated but is not required.
-- 2. Altered source versions must be plainly marked as such,
--    and must not be misrepresented as being the original software.
-- 3. This notice may not be removed or altered from any source distribution.
--//////////////////////////////////////////////////////////

--//////////////////////////////////////////////////////////


package Sf.Window.Clipboard is

   --//////////////////////////////////////////////////////////
   --//////////////////////////////////////////////////////////
   --//////////////////////////////////////////////////////////

   --//////////////////////////////////////////////////////////
   --//////////////////////////////////////////////////////////
   --/ @brief Get the content of the clipboard as string data (returns an ANSI string)
   --/
   --/ This function returns the content of the clipboard
   --/ as a string. If the clipboard does not contain string
   --/ it returns an empty string.
   --/
   --/ @return Clipboard contents as a locale-dependent ANSI string
   --/
   --//////////////////////////////////////////////////////////
   function getString return String;

   --//////////////////////////////////////////////////////////
   --/ @brief Get the content of the clipboard as string data (returns a Unicode string)
   --/
   --/ This function returns the content of the clipboard
   --/ as a string. If the clipboard does not contain string
   --/ it returns an empty string.
   --/
   --/ @return Clipboard contents as UTF-32
   --/
   --//////////////////////////////////////////////////////////
   function getUnicodeString return access sfUint32;

   --//////////////////////////////////////////////////////////
   --/ @brief Set the content of the clipboard as ANSI string data
   --/
   --/ This function sets the content of the clipboard as an
   --/ ANSI string.
   --/
   --/ @param text ANSI string containing the data to be sent
   --/ to the clipboard
   --/
   --//////////////////////////////////////////////////////////
   procedure setString (text : String);

   --//////////////////////////////////////////////////////////
   --/ @brief Set the content of the clipboard as Unicode string data
   --/
   --/ This function sets the content of the clipboard as a
   --/ Unicode string.
   --/
   --/ @param text Unicode string containing the data to be sent
   --/ to the clipboard
   --/
   --//////////////////////////////////////////////////////////
   procedure setUnicodeString (text : access sfUint32);

private

   pragma Import (C, getUnicodeString, "sfClipboard_getUnicodeString");
   pragma Import (C, setUnicodeString, "sfClipboard_setUnicodeString");

end Sf.Window.Clipboard;
