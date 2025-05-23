------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
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

--  $Id: aws-server.adb,v 1.1 2003/10/05 19:59:59 Jano Exp $

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with AWS.Config.Set;
with AWS.Dispatchers.Callback;
with AWS.Log;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.Net.SSL;
with AWS.OS_Lib;
with AWS.Session.Control;
with AWS.Status.Translate_Table;
with AWS.Templates;

package body AWS.Server is

   use Ada;

   Security_Initialized : Boolean := False;

   procedure Free is new Ada.Unchecked_Deallocation
     (Dispatchers.Handler'Class, Dispatchers.Handler_Class_Access);

   protected File_Upload_UID is
      procedure Get (ID : out Natural);
      --  returns a UID for file upload. This is to ensure that files
      --  coming from clients will always have different name.
   private
      UID : Natural := 0;
   end File_Upload_UID;

   procedure Start
     (Web_Server : in out HTTP;
      Dispatcher : in     Dispatchers.Handler'Class);
   --  Start web server with current configuration.

   procedure Protocol_Handler
     (HTTP_Server : in out HTTP;
      Index       : in     Positive);
   --  Handle the lines, this is where all the HTTP protocol is defined.

   function Accept_Socket_Serialized
     (Server : in HTTP_Access)
      return Net.Socket_Type'Class;
   --  Do a protected accept on the HTTP socket. It is not safe to call
   --  multiple accept on the same socket on some platforms.

   protected Counter is

      procedure Add;
      --  Add one to the server counter.

      procedure Remove;
      --  Removes one to the server counter.

      entry Zero;
      --  Accepted only when counter is equal to 0 (no more active server)

   private

      C : Natural := 0;

   end Counter;

   ------------------------------
   -- Accept_Socket_Serialized --
   ------------------------------

   function Accept_Socket_Serialized
     (Server : in HTTP_Access)
      return Net.Socket_Type'Class
   is
      New_Socket : Net.Socket_Type'Class
        := Net.Socket (CNF.Security (Server.Properties));
   begin
      Server.Sock_Sem.Seize;

      Net.Accept_Socket (Server.Sock, New_Socket);

      Server.Sock_Sem.Release;

      return New_Socket;

   exception
      when others =>
         Server.Sock_Sem.Release;
         raise;
   end Accept_Socket_Serialized;

   ------------
   -- Config --
   ------------

   function Config (Web_Server : in HTTP) return AWS.Config.Object is
   begin
      return Web_Server.Properties;
   end Config;

   -------------
   -- Counter --
   -------------

   protected body Counter is

      ---------
      -- Add --
      ---------

      procedure Add is
      begin
         C := C + 1;
      end Add;

      ------------
      -- Remove --
      ------------

      procedure Remove is
      begin
         C := C - 1;
      end Remove;

      ----------
      -- Zero --
      ----------

      entry Zero when C = 0 is
      begin
         null;
      end Zero;

   end Counter;

   ------------------------------------------
   -- Default_Unexpected_Exception_Handler --
   ------------------------------------------

   procedure Default_Unexpected_Exception_Handler
     (E      : in     Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : in     Exceptions.Data;
      Answer : in out Response.Data)
   is
      use Ada.Exceptions;
      use type Templates.Translate_Table;
      pragma Unreferenced (Log);

      Fatal_Error_Template  : constant String := "500.tmplt";
   begin
      if Error.Fatal then
         Text_IO.Put_Line
           (Text_IO.Current_Error, "Fatal error, slot"
              & Positive'Image (Error.Slot) & " is dead now.");
         Text_IO.New_Line (Text_IO.Current_Error);

         Text_IO.Put_Line
           (Text_IO.Current_Error, Exception_Information (E));

      else
         if AWS.OS_Lib.Is_Regular_File (Fatal_Error_Template) then
            Answer := Response.Build
              (MIME.Text_HTML,
               String'(Templates.Parse
                         (Fatal_Error_Template,
                          Status.Translate_Table (Error.Request)
                            & Templates.Assoc
                                ("EXCEPTION", Exception_Information (E)))),
               Messages.S500);
         else
            Answer := Response.Build
              (MIME.Text_HTML,
               "Internal Server Error.<br>"
                 & "Please, send the following information to the Web "
                 & "Master, thanks.<br><hr><br>"
                 & "<pre>" & Exception_Information (E) & "</pre>"
                 & "<br><hr>",
               Messages.S500);
         end if;
      end if;
   end Default_Unexpected_Exception_Handler;

   ---------------------
   -- File_Upload_UID --
   ---------------------

   protected body File_Upload_UID is

      ---------
      -- Get --
      ---------

      procedure Get (ID : out Natural) is
      begin
         ID  := UID;
         UID := UID + 1;
      end Get;

   end File_Upload_UID;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Web_Server : in out HTTP) is
   begin
      Shutdown (Web_Server);
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Web_Server : in out HTTP) is
      pragma Warnings (Off, Web_Server);
   begin
      null;
   end Initialize;

   ----------
   -- Line --
   ----------

   task body Line is

      HTTP_Server : HTTP_Access;
      Slot_Index  : Positive;

   begin

      select
         accept Start
           (Server : in HTTP;
            Index  : in Positive)
         do
            HTTP_Server := Server.Self;
            Slot_Index  := Index;
         end Start;
      or
         terminate;
      end select;

      --  Real job start here, we will exit only if there is an unrecoverable
      --  problem.

      while not HTTP_Server.Shutdown loop

         declare
            --  Wait for an incoming connection. Each call for the same server
            --  is serialized as some platforms do not handle properly
            --  multiple accepts on the same socket.

            Socket : aliased Net.Socket_Type'Class
              := Accept_Socket_Serialized (HTTP_Server);

            Free_Slots  : Natural;

         begin
            HTTP_Server.Slots.Set
              (Socket'Unchecked_Access,
               Slot_Index,
               Free_Slots);

            --  If there is no more slot available and we have many
            --  of them, try to abort one of them.

            if Free_Slots = 0
              and then CNF.Max_Connection (HTTP_Server.Properties) > 1
            then
               select
                  HTTP_Server.Cleaner.Force;
               or
                  delay 4.0;
                  Ada.Text_IO.Put_Line
                    (Text_IO.Current_Error, "Server too busy.");
               end select;
            end if;

            Protocol_Handler (HTTP_Server.all, Slot_Index);

            HTTP_Server.Slots.Release (Slot_Index);
         end;
      end loop;

   exception
      when E : others =>
         if not HTTP_Server.Shutdown then
            declare
               S      : Status.Data;
               pragma Warnings (Off, S);
               Answer : Response.Data;
            begin
               Log.Write
                 (HTTP_Server.Error_Log,
                  "Dead slot " & Utils.Image (Slot_Index) & ' '
                    & Utils.CRLF_2_Spaces
                        (Ada.Exceptions.Exception_Information (E)));

               HTTP_Server.Exception_Handler
                 (E, HTTP_Server.Error_Log, (True, Slot_Index, S), Answer);
            end;
         end if;
   end Line;

   ------------------
   -- Line_Cleaner --
   ------------------

   task body Line_Cleaner is
      Mode : Timeout_Mode;
      Done : Boolean := False;
   begin
      loop
         select
            accept Force do
               Mode := Force;
            end Force;
         or
            delay 30.0;
            Mode := Cleaner;
         end select;

         loop
            Server.Slots.Abort_On_Timeout (Mode, Done);

            exit when Mode /= Force or else Done;

            select
               accept Force;
            or
               delay 1.0;
            end select;
         end loop;

      end loop;
   end Line_Cleaner;

   ----------------------
   -- Protocol_Handler --
   ----------------------

   procedure Protocol_Handler
     (HTTP_Server : in out HTTP;
      Index       : in     Positive) is separate;

   ---------
   -- Set --
   ---------

   procedure Set
     (Web_Server : in out HTTP;
      Dispatcher : in     Dispatchers.Handler'Class)
   is
      Old : Dispatchers.Handler_Class_Access := Web_Server.Dispatcher;
   begin
      Web_Server.Dispatcher_Sem.Write;

      Web_Server.Dispatcher :=
        new Dispatchers.Handler'Class'(Dispatcher);

      Web_Server.Dispatcher_Sem.Release_Write;

      Free (Old);
   end Set;

   ------------------
   -- Set_Security --
   ------------------

   procedure Set_Security (Certificate_Filename : in String) is
   begin
      Security_Initialized := True;
      Net.SSL.Initialize (Certificate_Filename);
   end Set_Security;

   --------------------------------------
   -- Set_Unexpected_Exception_Handler --
   --------------------------------------

   procedure Set_Unexpected_Exception_Handler
     (Web_Server : in out HTTP;
      Handler    : in     Exceptions.Unexpected_Exception_Handler) is
   begin
      if Web_Server.Shutdown then
         Web_Server.Exception_Handler := Handler;
      else
         Ada.Exceptions.Raise_Exception (Constraint_Error'Identity,
            "Could not change exception handler on the active server.");
      end if;
   end Set_Unexpected_Exception_Handler;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Web_Server : in out HTTP) is

      procedure Free is
         new Ada.Unchecked_Deallocation (Line_Cleaner, Line_Cleaner_Access);

      procedure Free is
         new Ada.Unchecked_Deallocation (Line_Set, Line_Set_Access);

      procedure Free is
         new Ada.Unchecked_Deallocation (Slots, Slots_Access);

      procedure Free is
         new Ada.Unchecked_Deallocation (Line, Line_Access);

      All_Lines_Terminated : Boolean := False;

   begin
      if Web_Server.Shutdown then
         return;
      end if;

      Web_Server.Shutdown := True;

      --  First, close the sever socket, so no more request will be queued,
      --  furthermore this will help terminate all lines (see below).

      Net.Std.Shutdown (Web_Server.Sock);

      --  Release the slots

      for S in 1 .. Web_Server.Slots.N loop
         Web_Server.Slots.Shutdown (S);
      end loop;

      --  Wait for all lines to be terminated to be able to release associated
      --  memory.

      while not All_Lines_Terminated loop
         All_Lines_Terminated := True;

         for K in Web_Server.Lines'Range loop
            if not Web_Server.Lines (K)'Terminated then
               All_Lines_Terminated := False;
            end if;
         end loop;

         delay 0.5;
      end loop;

      --  Release the cleaner task

      abort Web_Server.Cleaner.all;

      --  Wait for Cleaner task to terminate to be able to release associated
      --  memory.

      while not Web_Server.Cleaner'Terminated loop
         delay 0.5;
      end loop;

      --  Release lines and slots memory

      for K in Web_Server.Lines'Range loop
         Free (Web_Server.Lines (K));
      end loop;

      Net.Std.Free (Web_Server.Sock);

      Free (Web_Server.Lines);

      Free (Web_Server.Cleaner);

      Free (Web_Server.Slots);

      Free (Web_Server.Dispatcher);

      --  Release the session server if needed

      if CNF.Session (Web_Server.Properties) then
         Session.Control.Shutdown;
      end if;

      --  Close logs, this ensure that all data will be written to the file.

      Stop_Log (Web_Server);

      Stop_Error_Log (Web_Server);

      --  Server removed

      Counter.Remove;
   end Shutdown;

   -----------
   -- Slots --
   -----------

   protected body Slots is

      ----------------------
      -- Abort_On_Timeout --
      ----------------------

      procedure Abort_On_Timeout
        (Mode : in Timeout_Mode; Done : out Boolean) is
      begin
         Done := False;

         for S in Table'Range loop
            if Is_Abortable (S, Mode) then
               Shutdown (S);
               Done := True;
            end if;
         end loop;
      end Abort_On_Timeout;

      ----------------
      -- Free_Slots --
      ----------------

      function Free_Slots return Natural is
      begin
         return Count;
      end Free_Slots;

      ---------
      -- Get --
      ---------

      function Get (Index : in Positive) return Slot is
      begin
         return Table (Index);
      end Get;

      ------------------
      -- Get_Peername --
      ------------------

      function Get_Peername (Index : in Positive) return String is
         use type Socket_Access;
         Socket : constant Socket_Access := Table (Index).Sock;
      begin
         if Socket = null then
            return "";
         else
            return Net.Peer_Addr (Socket.all);
         end if;
      end Get_Peername;

      ---------------------
      -- Get_Socket_Info --
      ---------------------

      function Get_Socket_Info (Index : in Positive) return Socket_Data is
         use type Socket_Access;
         Socket : constant Socket_Access := Table (Index).Sock;
      begin
         if Socket = null then
            return Socket_Data'
              (Peername_Length => 1, Peername => "-", FD => 0);

         else
            declare
               Peername : constant String := Net.Peer_Addr (Socket.all);
            begin
               return Socket_Data'
                  (Peername_Length => Peername'Length,
                   Peername        => Peername,
                   FD              => Net.Get_FD (Socket.all));
            end;
         end if;
      end Get_Socket_Info;

      -------------------------------------
      -- Increment_Slot_Activity_Counter --
      -------------------------------------

      procedure Increment_Slot_Activity_Counter (Index : in Positive) is
      begin
         Table (Index).Slot_Activity_Counter
           := Table (Index).Slot_Activity_Counter + 1;
         Table (Index).Alive_Counter
           := Table (Index).Alive_Counter + 1;
      end Increment_Slot_Activity_Counter;

      ------------------
      -- Is_Abortable --
      ------------------

      function Is_Abortable
        (Index : in Positive;
         Mode  : in Timeout_Mode)
         return Boolean
      is
         use type Calendar.Time;
         Phase : constant Slot_Phase    := Table (Index).Phase;
         Now   : constant Calendar.Time := Calendar.Clock;
      begin
         return
           (Phase in Abortable_Phase
            and then
            Now - Table (Index).Phase_Time_Stamp > Timeouts (Mode, Phase))

           or else

           (Phase in Data_Phase
            and then
            Now - Table (Index).Data_Time_Stamp > Data_Timeouts (Phase));
      end Is_Abortable;

      --------------------------
      -- Mark_Data_Time_Stamp --
      --------------------------

      procedure Mark_Data_Time_Stamp (Index : in Positive) is
      begin
         Table (Index).Data_Time_Stamp := Ada.Calendar.Clock;
      end Mark_Data_Time_Stamp;

      ----------------
      -- Mark_Phase --
      ----------------

      procedure Mark_Phase (Index : in Positive; Phase : in Slot_Phase) is
      begin
         --  Check if the Aborted phase happen between after socket operation
         --  and before Mark_Phase call.

         if Table (Index).Phase = Aborted
           and then Phase /= Closed
         then
            raise Net.Socket_Error;
         end if;

         Table (Index).Phase_Time_Stamp := Ada.Calendar.Clock;
         Table (Index).Phase := Phase;

         if Phase in Data_Phase then
            Mark_Data_Time_Stamp (Index);
         end if;
      end Mark_Phase;

      -------------
      -- Release --
      -------------

      procedure Release (Index : in Positive) is
         use type Socket_Access;
      begin
         pragma Assert (Count < N);
         --  No more release than it is possible
         pragma Assert
           ((Table (Index).Phase = Closed
               and then -- If phase is closed, then Sock must be null
               (Table (Index).Sock = null))
            or else -- or phase is not closed
              (Table (Index).Phase /= Closed));

         Count := Count + 1;

         if Table (Index).Phase /= Closed then

            if not Table (Index).Socket_Taken  then

               if Table (Index).Phase /= Aborted then
                  begin
                     --  This must never fail, it is possible that Shutdown
                     --  raise Socket_Error if the slot has been aborted by
                     --  the browser for example.
                     Net.Shutdown (Table (Index).Sock.all);
                  exception
                     when Net.Socket_Error =>
                        null;
                  end;
               end if;

               Net.Free (Table (Index).Sock.all);
            else

               Table (Index).Socket_Taken := False;
            end if;

            Mark_Phase (Index, Closed);
            Table (Index).Sock := null;
         end if;
      end Release;

      ---------
      -- Set --
      ---------

      procedure Set
        (Socket     : in     Socket_Access;
         Index      : in     Positive;
         Free_Slots :    out Natural) is
      begin
         pragma Assert (Count > 0);

         Table (Index).Sock := Socket;
         Mark_Phase (Index, Wait_For_Client);
         Table (Index).Alive_Counter := 0;
         Table (Index).Alive_Time_Stamp := Ada.Calendar.Clock;
         Table (Index).Activity_Counter := Table (Index).Activity_Counter + 1;
         Count := Count - 1;
         Free_Slots := Count;
      end Set;

      ------------------
      -- Set_Timeouts --
      ------------------

      procedure Set_Timeouts
        (Phase_Timeouts : in Timeouts_Array;
         Data_Timeouts  : in Data_Timeouts_Array) is
      begin
         Timeouts := Phase_Timeouts;
         Slots.Data_Timeouts := Set_Timeouts.Data_Timeouts;
      end Set_Timeouts;

      --------------
      -- Shutdown --
      --------------

      procedure Shutdown (Index : in Positive) is
      begin
         if Table (Index).Phase not in Closed .. Aborted then
            Mark_Phase (Index, Aborted);
            Net.Shutdown (Table (Index).Sock.all);
         end if;
      end Shutdown;

      ------------------
      -- Socket_Taken --
      ------------------

      procedure Socket_Taken (Index : in Positive) is
      begin
         Table (Index).Socket_Taken := True;
      end Socket_Taken;

   end Slots;

   -----------
   -- Start --
   -----------

   procedure Start
     (Web_Server                : in out HTTP;
      Name                      : in     String;
      Callback                  : in     Response.Callback;
      Max_Connection            : in     Positive  := Default.Max_Connection;
      Admin_URI                 : in     String    := Default.Admin_URI;
      Port                      : in     Positive  := Default.Server_Port;
      Security                  : in     Boolean   := False;
      Session                   : in     Boolean   := False;
      Case_Sensitive_Parameters : in     Boolean   := True;
      Upload_Directory          : in     String    := Default.Upload_Directory;
      Line_Stack_Size           : in     Positive  := Default.Line_Stack_Size)
   is
   begin
      CNF.Set.Server_Name      (Web_Server.Properties, Name);
      CNF.Set.Admin_URI        (Web_Server.Properties, Admin_URI);
      CNF.Set.Server_Port      (Web_Server.Properties, Port);
      CNF.Set.Security         (Web_Server.Properties, Security);
      CNF.Set.Session          (Web_Server.Properties, Session);
      CNF.Set.Upload_Directory (Web_Server.Properties, Upload_Directory);
      CNF.Set.Max_Connection   (Web_Server.Properties, Max_Connection);
      CNF.Set.Line_Stack_Size  (Web_Server.Properties, Line_Stack_Size);

      CNF.Set.Case_Sensitive_Parameters
        (Web_Server.Properties, Case_Sensitive_Parameters);

      Start (Web_Server, Dispatchers.Callback.Create (Callback));
   end Start;

   -----------
   -- Start --
   -----------

   procedure Start
     (Web_Server : in out HTTP;
      Callback   : in     Response.Callback;
      Config     : in     AWS.Config.Object) is
   begin
      Web_Server.Properties := Config;
      Start (Web_Server, Dispatchers.Callback.Create (Callback));
   end Start;

   -----------
   -- Start --
   -----------

   procedure Start
     (Web_Server : in out HTTP;
      Dispatcher : in     Dispatchers.Handler'Class;
      Config     : in     AWS.Config.Object) is
   begin
      Web_Server.Properties := Config;
      Start (Web_Server, Dispatcher);
   end Start;

   -----------
   -- Start --
   -----------

   procedure Start
     (Web_Server : in out HTTP;
      Dispatcher : in     Dispatchers.Handler'Class)
   is
      Max_Connection : constant Positive
        := CNF.Max_Connection (Web_Server.Properties);

   begin
      --  If it is an SSL connection, initialize the SSL library

      if not Security_Initialized
        and then CNF.Security (Web_Server.Properties)
      then
         Security_Initialized := True;
         Net.SSL.Initialize (CNF.Certificate);
      end if;

      Net.Std.Bind
        (Web_Server.Sock,
         CNF.Server_Port (Web_Server.Properties),
         CNF.Server_Host (Web_Server.Properties));

      Net.Std.Listen
        (Web_Server.Sock,
         Queue_Size => CNF.Accept_Queue_Size (Web_Server.Properties));

      Web_Server.Dispatcher := new Dispatchers.Handler'Class'(Dispatcher);

      --  Initialize slots

      Web_Server.Slots := new Slots (Max_Connection);

      --  Set timeouts

      Web_Server.Slots.Set_Timeouts
        ((Cleaner => -- Timeouts for Line_Cleaner
            (Wait_For_Client  =>
               CNF.Cleaner_Wait_For_Client_Timeout (Web_Server.Properties),
             Client_Header    =>
               CNF.Cleaner_Client_Header_Timeout (Web_Server.Properties),
             Client_Data      =>
               CNF.Cleaner_Client_Data_Timeout (Web_Server.Properties),
             Server_Response  =>
               CNF.Cleaner_Server_Response_Timeout (Web_Server.Properties)),

          Force   => -- Force timeouts used when there is no free slot
            (Wait_For_Client  =>
               CNF.Force_Wait_For_Client_Timeout (Web_Server.Properties),
             Client_Header    =>
               CNF.Force_Client_Header_Timeout (Web_Server.Properties),
             Client_Data      =>
               CNF.Force_Client_Data_Timeout (Web_Server.Properties),
             Server_Response  =>
               CNF.Cleaner_Server_Response_Timeout (Web_Server.Properties))),

         (Client_Data     =>
            CNF.Receive_Timeout (Web_Server.Properties),
          Server_Response =>
            CNF.Send_Timeout (Web_Server.Properties)));

      --  Started time

      Web_Server.Start_Time := Calendar.Clock;

      --  Initialize the connection lines

      Web_Server.Lines := new Line_Set'
        (1 .. Max_Connection
         => new Line (CNF.Line_Stack_Size (Web_Server.Properties)));

      --  Initialize the cleaner task

      Web_Server.Cleaner := new Line_Cleaner (Web_Server.Self);

      --  Set Shutdown to False here since it must be done before starting the
      --  lines.

      Web_Server.Shutdown := False;

      --  Start each connection lines.

      for I in 1 .. Max_Connection loop
         Web_Server.Lines (I).Start (Web_Server, I);
      end loop;

      --  Initialize session server.

      if AWS.Config.Session (Web_Server.Properties) then
         AWS.Session.Control.Start
           (Session_Check_Interval => CNF.Session_Cleanup_Interval,
            Session_Lifetime       => CNF.Session_Lifetime);
      end if;

      Counter.Add;
   end Start;

   ---------------------
   -- Start_Error_Log --
   ---------------------

   procedure Start_Error_Log
     (Web_Server        : in out HTTP;
      Split_Mode        : in     Log.Split_Mode := Log.None;
      Filename_Prefix   : in     String         := "")
   is
      use type AWS.Log.Split_Mode;
   begin
      if Split_Mode /= Log.None then
         CNF.Set.Error_Log_Split_Mode
           (Web_Server.Properties, Log.Split_Mode'Image (Split_Mode));
      end if;

      if Filename_Prefix /= "" then
         CNF.Set.Error_Log_Filename_Prefix
           (Web_Server.Properties, Filename_Prefix);
      end if;

      Log.Start
        (Web_Server.Error_Log,
         Log.Split_Mode'Value
           (CNF.Error_Log_Split_Mode (Web_Server.Properties)),
         CNF.Log_File_Directory (Web_Server.Properties),
         CNF.Error_Log_Filename_Prefix (Web_Server.Properties));
   end Start_Error_Log;

   ---------------
   -- Start_Log --
   ---------------

   procedure Start_Log
     (Web_Server        : in out HTTP;
      Split_Mode        : in     Log.Split_Mode := Log.None;
      Filename_Prefix   : in     String         := "")
   is
      use type AWS.Log.Split_Mode;
   begin
      if Split_Mode /= Log.None then
         CNF.Set.Log_Split_Mode
           (Web_Server.Properties, Log.Split_Mode'Image (Split_Mode));
      end if;

      if Filename_Prefix /= "" then
         CNF.Set.Log_Filename_Prefix
           (Web_Server.Properties, Filename_Prefix);
      end if;

      Log.Start
        (Web_Server.Log,
         Log.Split_Mode'Value (CNF.Log_Split_Mode (Web_Server.Properties)),
         CNF.Log_File_Directory (Web_Server.Properties),
         CNF.Log_Filename_Prefix (Web_Server.Properties));
   end Start_Log;

   --------------------
   -- Stop_Error_Log --
   --------------------

   procedure Stop_Error_Log (Web_Server : in out HTTP) is
   begin
      Log.Stop (Web_Server.Error_Log);
   end Stop_Error_Log;

   --------------
   -- Stop_Log --
   --------------

   procedure Stop_Log (Web_Server : in out HTTP) is
   begin
      Log.Stop (Web_Server.Log);
   end Stop_Log;

   ----------
   -- Wait --
   ----------

   procedure Wait (Mode : in Termination := No_Server) is
   begin
      case Mode is
         when No_Server =>
            Counter.Zero;

         when Q_Key_Pressed =>
            declare
               K : Character;
            begin
               loop
                  Text_IO.Get_Immediate (K);
                  exit when K = 'q' or else K = 'Q';
               end loop;
            end;

         when Forever =>
            loop
               delay Duration'Last;
            end loop;
      end case;
   end Wait;

end AWS.Server;
