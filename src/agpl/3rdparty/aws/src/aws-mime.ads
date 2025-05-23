------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id: aws-mime.ads,v 1.1 2003/10/05 19:59:56 Jano Exp $

package AWS.MIME is

   Text_HTML           : aliased constant String := "text/html";
   Text_XML            : aliased constant String := "text/xml";
   Text_Plain          : aliased constant String := "text/plain";

   Image_Gif           : aliased constant String := "image/gif";
   Image_Jpeg          : aliased constant String := "image/jpeg";
   Image_Png           : aliased constant String := "image/png";

   Appl_Postscript     : aliased constant String := "application/postscript";
   Appl_Pdf            : aliased constant String := "application/pdf";
   Appl_Zip            : aliased constant String := "application/zip";
   Appl_Octet_Stream   : aliased constant String := "application/octet-stream";

   Appl_Form_Data      : aliased constant String
     := "application/x-www-form-urlencoded";

   Multipart_Form_Data : aliased constant String := "multipart/form-data";

   Multipart_Mixed_Replace : constant String := "multipart/x-mixed-replace";

   function Content_Type (Filename : in String) return String;
   --  Determine the MIME Content Type from the file's type extension.
   --  Returns "application/octet-stream" if the file type is unknown.

   function Is_Text (Mime_Type : in String) return Boolean;
   --  Returns True if the Mime_Type is a kind of textual data.

end AWS.MIME;
