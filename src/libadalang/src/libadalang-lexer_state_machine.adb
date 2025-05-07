--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

package body Libadalang.Lexer_State_Machine is

   Is_Trivia : constant array (Token_Kind) of Boolean := (
      Ada_Termination => False, Ada_Lexing_Failure => True, Ada_Identifier => False, Ada_All => False, Ada_Abort => False, Ada_Else => False, Ada_New => False, Ada_Return => False, Ada_Abs => False, Ada_Elsif => False, Ada_Not => False, Ada_Reverse => False, Ada_End => False, Ada_Null => False, Ada_Accept => False, Ada_Entry => False, Ada_Select => False, Ada_Access => False, Ada_Exception => False, Ada_Of => False, Ada_Separate => False, Ada_Exit => False, Ada_Or => False, Ada_Others => False, Ada_Subtype => False, Ada_And => False, Ada_For => False, Ada_Out => False, Ada_Array => False, Ada_Function => False, Ada_At => False, Ada_Generic => False, Ada_Package => False, Ada_Task => False, Ada_Begin => False, Ada_Goto => False, Ada_Pragma => False, Ada_Terminate => False, Ada_Body => False, Ada_Private => False, Ada_Then => False, Ada_If => False, Ada_Procedure => False, Ada_Type => False, Ada_Case => False, Ada_In => False, Ada_Constant => False, Ada_Is => False, Ada_Raise => False, Ada_Use => False, Ada_Declare => False, Ada_Range => False, Ada_Delay => False, Ada_Limited => False, Ada_Record => False, Ada_When => False, Ada_Delta => False, Ada_Loop => False, Ada_Rem => False, Ada_While => False, Ada_Digits => False, Ada_Renames => False, Ada_Do => False, Ada_Mod => False, Ada_Xor => False, Ada_Par_Close => False, Ada_Par_Open => False, Ada_Brack_Close => False, Ada_Brack_Open => False, Ada_Semicolon => False, Ada_Colon => False, Ada_Comma => False, Ada_Doubledot => False, Ada_Dot => False, Ada_Diamond => False, Ada_Lte => False, Ada_Gte => False, Ada_Arrow => False, Ada_Equal => False, Ada_Lt => False, Ada_Gt => False, Ada_Plus => False, Ada_Minus => False, Ada_Power => False, Ada_Mult => False, Ada_Amp => False, Ada_Notequal => False, Ada_Divide => False, Ada_Tick => False, Ada_Pipe => False, Ada_Assign => False, Ada_Label_Start => False, Ada_Label_End => False, Ada_Target => False, Ada_String => False, Ada_Char => False, Ada_With => False, Ada_Decimal => False, Ada_Integer => False, Ada_Comment => True, Ada_Prep_Line => True, Ada_Whitespace => True
   );

   type Character_Range is record
      First, Last : Character_Type;
   end record;

   type Character_Range_Array is array (Positive range <>) of Character_Range;
   --  Sorted list of dijoint character ranges

   pragma Warnings (Off, "referenced");
   function Contains
     (Char : Character_Type; Ranges : Character_Range_Array) return Boolean;
   --  Return whether Char is included in the given ranges
   pragma Warnings (On, "referenced");

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : out Lexer_State;
      Input       : Text_Access;
      Input_First : Positive;
      Input_Last  : Natural) is
   begin
      Self.Input := Input;
      Self.Input_First := Input_First;
      Self.Input_Last := Input_Last;
      Self.Has_Next := True;
      Self.Last_Token := (Kind       => Ada_Termination,
                          Text_First => Input_First,
                          Text_Last  => Input_First - 1);
      Self.Last_Token_Kind := Ada_Termination;
   end Initialize;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Self : Lexer_State) return Lexed_Token is
   begin
      return Self.Last_Token;
   end Last_Token;

   --------------
   -- Has_Next --
   --------------

   function Has_Next (Self : Lexer_State) return Boolean is
   begin
      return Self.Has_Next;
   end Has_Next;

   --------------
   -- Contains --
   --------------

   function Contains
     (Char : Character_Type; Ranges : Character_Range_Array) return Boolean
   is
      Low  : Natural := Ranges'First;
      High : Natural := Ranges'Last;
   begin
      while Low <= High loop
         declare
            Middle : constant Natural := (Low + High) / 2;
            R      : Character_Range renames Ranges (Middle);
         begin
            if Char < R.First then
               High := Middle - 1;
            elsif Char > R.Last then
               Low := Middle + 1;
            else
               return True;
            end if;
         end;
      end loop;
      return False;
   end Contains;

   Ranges_0 : constant Character_Range_Array := (
   (Character_Type'Val (170), Character_Type'Val (170)),
   (Character_Type'Val (181), Character_Type'Val (181)),
   (Character_Type'Val (186), Character_Type'Val (186)),
   (Character_Type'Val (192), Character_Type'Val (214)),
   (Character_Type'Val (216), Character_Type'Val (246)),
   (Character_Type'Val (248), Character_Type'Val (442)),
   (Character_Type'Val (444), Character_Type'Val (447)),
   (Character_Type'Val (452), Character_Type'Val (659)),
   (Character_Type'Val (661), Character_Type'Val (687)),
   (Character_Type'Val (880), Character_Type'Val (883)),
   (Character_Type'Val (886), Character_Type'Val (887)),
   (Character_Type'Val (891), Character_Type'Val (893)),
   (Character_Type'Val (902), Character_Type'Val (902)),
   (Character_Type'Val (904), Character_Type'Val (906)),
   (Character_Type'Val (908), Character_Type'Val (908)),
   (Character_Type'Val (910), Character_Type'Val (929)),
   (Character_Type'Val (931), Character_Type'Val (1013)),
   (Character_Type'Val (1015), Character_Type'Val (1153)),
   (Character_Type'Val (1162), Character_Type'Val (1317)),
   (Character_Type'Val (1329), Character_Type'Val (1366)),
   (Character_Type'Val (1377), Character_Type'Val (1415)),
   (Character_Type'Val (4256), Character_Type'Val (4293)),
   (Character_Type'Val (7424), Character_Type'Val (7467)),
   (Character_Type'Val (7522), Character_Type'Val (7543)),
   (Character_Type'Val (7545), Character_Type'Val (7578)),
   (Character_Type'Val (7680), Character_Type'Val (7957)),
   (Character_Type'Val (7960), Character_Type'Val (7965)),
   (Character_Type'Val (7968), Character_Type'Val (8005)),
   (Character_Type'Val (8008), Character_Type'Val (8013)),
   (Character_Type'Val (8016), Character_Type'Val (8023)),
   (Character_Type'Val (8025), Character_Type'Val (8025)),
   (Character_Type'Val (8027), Character_Type'Val (8027)),
   (Character_Type'Val (8029), Character_Type'Val (8029)),
   (Character_Type'Val (8031), Character_Type'Val (8061)),
   (Character_Type'Val (8064), Character_Type'Val (8116)),
   (Character_Type'Val (8118), Character_Type'Val (8124)),
   (Character_Type'Val (8126), Character_Type'Val (8126)),
   (Character_Type'Val (8130), Character_Type'Val (8132)),
   (Character_Type'Val (8134), Character_Type'Val (8140)),
   (Character_Type'Val (8144), Character_Type'Val (8147)),
   (Character_Type'Val (8150), Character_Type'Val (8155)),
   (Character_Type'Val (8160), Character_Type'Val (8172)),
   (Character_Type'Val (8178), Character_Type'Val (8180)),
   (Character_Type'Val (8182), Character_Type'Val (8188)),
   (Character_Type'Val (8450), Character_Type'Val (8450)),
   (Character_Type'Val (8455), Character_Type'Val (8455)),
   (Character_Type'Val (8458), Character_Type'Val (8467)),
   (Character_Type'Val (8469), Character_Type'Val (8469)),
   (Character_Type'Val (8473), Character_Type'Val (8477)),
   (Character_Type'Val (8484), Character_Type'Val (8484)),
   (Character_Type'Val (8486), Character_Type'Val (8486)),
   (Character_Type'Val (8488), Character_Type'Val (8488)),
   (Character_Type'Val (8490), Character_Type'Val (8493)),
   (Character_Type'Val (8495), Character_Type'Val (8500)),
   (Character_Type'Val (8505), Character_Type'Val (8505)),
   (Character_Type'Val (8508), Character_Type'Val (8511)),
   (Character_Type'Val (8517), Character_Type'Val (8521)),
   (Character_Type'Val (8526), Character_Type'Val (8526)),
   (Character_Type'Val (8579), Character_Type'Val (8580)),
   (Character_Type'Val (11264), Character_Type'Val (11310)),
   (Character_Type'Val (11312), Character_Type'Val (11358)),
   (Character_Type'Val (11360), Character_Type'Val (11388)),
   (Character_Type'Val (11390), Character_Type'Val (11492)),
   (Character_Type'Val (11499), Character_Type'Val (11502)),
   (Character_Type'Val (11520), Character_Type'Val (11557)),
   (Character_Type'Val (42560), Character_Type'Val (42591)),
   (Character_Type'Val (42594), Character_Type'Val (42605)),
   (Character_Type'Val (42624), Character_Type'Val (42647)),
   (Character_Type'Val (42786), Character_Type'Val (42863)),
   (Character_Type'Val (42865), Character_Type'Val (42887)),
   (Character_Type'Val (42891), Character_Type'Val (42892)),
   (Character_Type'Val (64256), Character_Type'Val (64262)),
   (Character_Type'Val (64275), Character_Type'Val (64279)),
   (Character_Type'Val (65313), Character_Type'Val (65338)),
   (Character_Type'Val (65345), Character_Type'Val (65370)),
   (Character_Type'Val (66560), Character_Type'Val (66639)),
   (Character_Type'Val (119808), Character_Type'Val (119892)),
   (Character_Type'Val (119894), Character_Type'Val (119964)),
   (Character_Type'Val (119966), Character_Type'Val (119967)),
   (Character_Type'Val (119970), Character_Type'Val (119970)),
   (Character_Type'Val (119973), Character_Type'Val (119974)),
   (Character_Type'Val (119977), Character_Type'Val (119980)),
   (Character_Type'Val (119982), Character_Type'Val (119993)),
   (Character_Type'Val (119995), Character_Type'Val (119995)),
   (Character_Type'Val (119997), Character_Type'Val (120003)),
   (Character_Type'Val (120005), Character_Type'Val (120069)),
   (Character_Type'Val (120071), Character_Type'Val (120074)),
   (Character_Type'Val (120077), Character_Type'Val (120084)),
   (Character_Type'Val (120086), Character_Type'Val (120092)),
   (Character_Type'Val (120094), Character_Type'Val (120121)),
   (Character_Type'Val (120123), Character_Type'Val (120126)),
   (Character_Type'Val (120128), Character_Type'Val (120132)),
   (Character_Type'Val (120134), Character_Type'Val (120134)),
   (Character_Type'Val (120138), Character_Type'Val (120144)),
   (Character_Type'Val (120146), Character_Type'Val (120485)),
   (Character_Type'Val (120488), Character_Type'Val (120512)),
   (Character_Type'Val (120514), Character_Type'Val (120538)),
   (Character_Type'Val (120540), Character_Type'Val (120570)),
   (Character_Type'Val (120572), Character_Type'Val (120596)),
   (Character_Type'Val (120598), Character_Type'Val (120628)),
   (Character_Type'Val (120630), Character_Type'Val (120654)),
   (Character_Type'Val (120656), Character_Type'Val (120686)),
   (Character_Type'Val (120688), Character_Type'Val (120712)),
   (Character_Type'Val (120714), Character_Type'Val (120744)),
   (Character_Type'Val (120746), Character_Type'Val (120770)),
   (Character_Type'Val (120772), Character_Type'Val (120779))
   );
   Ranges_1 : constant Character_Range_Array := (
   (Character_Type'Val (443), Character_Type'Val (443)),
   (Character_Type'Val (448), Character_Type'Val (451)),
   (Character_Type'Val (660), Character_Type'Val (660)),
   (Character_Type'Val (1488), Character_Type'Val (1514)),
   (Character_Type'Val (1520), Character_Type'Val (1522)),
   (Character_Type'Val (1569), Character_Type'Val (1599)),
   (Character_Type'Val (1601), Character_Type'Val (1610)),
   (Character_Type'Val (1646), Character_Type'Val (1647)),
   (Character_Type'Val (1649), Character_Type'Val (1747)),
   (Character_Type'Val (1749), Character_Type'Val (1749)),
   (Character_Type'Val (1774), Character_Type'Val (1775)),
   (Character_Type'Val (1786), Character_Type'Val (1788)),
   (Character_Type'Val (1791), Character_Type'Val (1791)),
   (Character_Type'Val (1808), Character_Type'Val (1808)),
   (Character_Type'Val (1810), Character_Type'Val (1839)),
   (Character_Type'Val (1869), Character_Type'Val (1957)),
   (Character_Type'Val (1969), Character_Type'Val (1969)),
   (Character_Type'Val (1994), Character_Type'Val (2026)),
   (Character_Type'Val (2048), Character_Type'Val (2069)),
   (Character_Type'Val (2308), Character_Type'Val (2361)),
   (Character_Type'Val (2365), Character_Type'Val (2365)),
   (Character_Type'Val (2384), Character_Type'Val (2384)),
   (Character_Type'Val (2392), Character_Type'Val (2401)),
   (Character_Type'Val (2418), Character_Type'Val (2418)),
   (Character_Type'Val (2425), Character_Type'Val (2431)),
   (Character_Type'Val (2437), Character_Type'Val (2444)),
   (Character_Type'Val (2447), Character_Type'Val (2448)),
   (Character_Type'Val (2451), Character_Type'Val (2472)),
   (Character_Type'Val (2474), Character_Type'Val (2480)),
   (Character_Type'Val (2482), Character_Type'Val (2482)),
   (Character_Type'Val (2486), Character_Type'Val (2489)),
   (Character_Type'Val (2493), Character_Type'Val (2493)),
   (Character_Type'Val (2510), Character_Type'Val (2510)),
   (Character_Type'Val (2524), Character_Type'Val (2525)),
   (Character_Type'Val (2527), Character_Type'Val (2529)),
   (Character_Type'Val (2544), Character_Type'Val (2545)),
   (Character_Type'Val (2565), Character_Type'Val (2570)),
   (Character_Type'Val (2575), Character_Type'Val (2576)),
   (Character_Type'Val (2579), Character_Type'Val (2600)),
   (Character_Type'Val (2602), Character_Type'Val (2608)),
   (Character_Type'Val (2610), Character_Type'Val (2611)),
   (Character_Type'Val (2613), Character_Type'Val (2614)),
   (Character_Type'Val (2616), Character_Type'Val (2617)),
   (Character_Type'Val (2649), Character_Type'Val (2652)),
   (Character_Type'Val (2654), Character_Type'Val (2654)),
   (Character_Type'Val (2674), Character_Type'Val (2676)),
   (Character_Type'Val (2693), Character_Type'Val (2701)),
   (Character_Type'Val (2703), Character_Type'Val (2705)),
   (Character_Type'Val (2707), Character_Type'Val (2728)),
   (Character_Type'Val (2730), Character_Type'Val (2736)),
   (Character_Type'Val (2738), Character_Type'Val (2739)),
   (Character_Type'Val (2741), Character_Type'Val (2745)),
   (Character_Type'Val (2749), Character_Type'Val (2749)),
   (Character_Type'Val (2768), Character_Type'Val (2768)),
   (Character_Type'Val (2784), Character_Type'Val (2785)),
   (Character_Type'Val (2821), Character_Type'Val (2828)),
   (Character_Type'Val (2831), Character_Type'Val (2832)),
   (Character_Type'Val (2835), Character_Type'Val (2856)),
   (Character_Type'Val (2858), Character_Type'Val (2864)),
   (Character_Type'Val (2866), Character_Type'Val (2867)),
   (Character_Type'Val (2869), Character_Type'Val (2873)),
   (Character_Type'Val (2877), Character_Type'Val (2877)),
   (Character_Type'Val (2908), Character_Type'Val (2909)),
   (Character_Type'Val (2911), Character_Type'Val (2913)),
   (Character_Type'Val (2929), Character_Type'Val (2929)),
   (Character_Type'Val (2947), Character_Type'Val (2947)),
   (Character_Type'Val (2949), Character_Type'Val (2954)),
   (Character_Type'Val (2958), Character_Type'Val (2960)),
   (Character_Type'Val (2962), Character_Type'Val (2965)),
   (Character_Type'Val (2969), Character_Type'Val (2970)),
   (Character_Type'Val (2972), Character_Type'Val (2972)),
   (Character_Type'Val (2974), Character_Type'Val (2975)),
   (Character_Type'Val (2979), Character_Type'Val (2980)),
   (Character_Type'Val (2984), Character_Type'Val (2986)),
   (Character_Type'Val (2990), Character_Type'Val (3001)),
   (Character_Type'Val (3024), Character_Type'Val (3024)),
   (Character_Type'Val (3077), Character_Type'Val (3084)),
   (Character_Type'Val (3086), Character_Type'Val (3088)),
   (Character_Type'Val (3090), Character_Type'Val (3112)),
   (Character_Type'Val (3114), Character_Type'Val (3123)),
   (Character_Type'Val (3125), Character_Type'Val (3129)),
   (Character_Type'Val (3133), Character_Type'Val (3133)),
   (Character_Type'Val (3160), Character_Type'Val (3161)),
   (Character_Type'Val (3168), Character_Type'Val (3169)),
   (Character_Type'Val (3205), Character_Type'Val (3212)),
   (Character_Type'Val (3214), Character_Type'Val (3216)),
   (Character_Type'Val (3218), Character_Type'Val (3240)),
   (Character_Type'Val (3242), Character_Type'Val (3251)),
   (Character_Type'Val (3253), Character_Type'Val (3257)),
   (Character_Type'Val (3261), Character_Type'Val (3261)),
   (Character_Type'Val (3294), Character_Type'Val (3294)),
   (Character_Type'Val (3296), Character_Type'Val (3297)),
   (Character_Type'Val (3333), Character_Type'Val (3340)),
   (Character_Type'Val (3342), Character_Type'Val (3344)),
   (Character_Type'Val (3346), Character_Type'Val (3368)),
   (Character_Type'Val (3370), Character_Type'Val (3385)),
   (Character_Type'Val (3389), Character_Type'Val (3389)),
   (Character_Type'Val (3424), Character_Type'Val (3425)),
   (Character_Type'Val (3450), Character_Type'Val (3455)),
   (Character_Type'Val (3461), Character_Type'Val (3478)),
   (Character_Type'Val (3482), Character_Type'Val (3505)),
   (Character_Type'Val (3507), Character_Type'Val (3515)),
   (Character_Type'Val (3517), Character_Type'Val (3517)),
   (Character_Type'Val (3520), Character_Type'Val (3526)),
   (Character_Type'Val (3585), Character_Type'Val (3632)),
   (Character_Type'Val (3634), Character_Type'Val (3635)),
   (Character_Type'Val (3648), Character_Type'Val (3653)),
   (Character_Type'Val (3713), Character_Type'Val (3714)),
   (Character_Type'Val (3716), Character_Type'Val (3716)),
   (Character_Type'Val (3719), Character_Type'Val (3720)),
   (Character_Type'Val (3722), Character_Type'Val (3722)),
   (Character_Type'Val (3725), Character_Type'Val (3725)),
   (Character_Type'Val (3732), Character_Type'Val (3735)),
   (Character_Type'Val (3737), Character_Type'Val (3743)),
   (Character_Type'Val (3745), Character_Type'Val (3747)),
   (Character_Type'Val (3749), Character_Type'Val (3749)),
   (Character_Type'Val (3751), Character_Type'Val (3751)),
   (Character_Type'Val (3754), Character_Type'Val (3755)),
   (Character_Type'Val (3757), Character_Type'Val (3760)),
   (Character_Type'Val (3762), Character_Type'Val (3763)),
   (Character_Type'Val (3773), Character_Type'Val (3773)),
   (Character_Type'Val (3776), Character_Type'Val (3780)),
   (Character_Type'Val (3804), Character_Type'Val (3805)),
   (Character_Type'Val (3840), Character_Type'Val (3840)),
   (Character_Type'Val (3904), Character_Type'Val (3911)),
   (Character_Type'Val (3913), Character_Type'Val (3948)),
   (Character_Type'Val (3976), Character_Type'Val (3979)),
   (Character_Type'Val (4096), Character_Type'Val (4138)),
   (Character_Type'Val (4159), Character_Type'Val (4159)),
   (Character_Type'Val (4176), Character_Type'Val (4181)),
   (Character_Type'Val (4186), Character_Type'Val (4189)),
   (Character_Type'Val (4193), Character_Type'Val (4193)),
   (Character_Type'Val (4197), Character_Type'Val (4198)),
   (Character_Type'Val (4206), Character_Type'Val (4208)),
   (Character_Type'Val (4213), Character_Type'Val (4225)),
   (Character_Type'Val (4238), Character_Type'Val (4238)),
   (Character_Type'Val (4304), Character_Type'Val (4346)),
   (Character_Type'Val (4352), Character_Type'Val (4680)),
   (Character_Type'Val (4682), Character_Type'Val (4685)),
   (Character_Type'Val (4688), Character_Type'Val (4694)),
   (Character_Type'Val (4696), Character_Type'Val (4696)),
   (Character_Type'Val (4698), Character_Type'Val (4701)),
   (Character_Type'Val (4704), Character_Type'Val (4744)),
   (Character_Type'Val (4746), Character_Type'Val (4749)),
   (Character_Type'Val (4752), Character_Type'Val (4784)),
   (Character_Type'Val (4786), Character_Type'Val (4789)),
   (Character_Type'Val (4792), Character_Type'Val (4798)),
   (Character_Type'Val (4800), Character_Type'Val (4800)),
   (Character_Type'Val (4802), Character_Type'Val (4805)),
   (Character_Type'Val (4808), Character_Type'Val (4822)),
   (Character_Type'Val (4824), Character_Type'Val (4880)),
   (Character_Type'Val (4882), Character_Type'Val (4885)),
   (Character_Type'Val (4888), Character_Type'Val (4954)),
   (Character_Type'Val (4992), Character_Type'Val (5007)),
   (Character_Type'Val (5024), Character_Type'Val (5108)),
   (Character_Type'Val (5121), Character_Type'Val (5740)),
   (Character_Type'Val (5743), Character_Type'Val (5759)),
   (Character_Type'Val (5761), Character_Type'Val (5786)),
   (Character_Type'Val (5792), Character_Type'Val (5866)),
   (Character_Type'Val (5888), Character_Type'Val (5900)),
   (Character_Type'Val (5902), Character_Type'Val (5905)),
   (Character_Type'Val (5920), Character_Type'Val (5937)),
   (Character_Type'Val (5952), Character_Type'Val (5969)),
   (Character_Type'Val (5984), Character_Type'Val (5996)),
   (Character_Type'Val (5998), Character_Type'Val (6000)),
   (Character_Type'Val (6016), Character_Type'Val (6067)),
   (Character_Type'Val (6108), Character_Type'Val (6108)),
   (Character_Type'Val (6176), Character_Type'Val (6210)),
   (Character_Type'Val (6212), Character_Type'Val (6263)),
   (Character_Type'Val (6272), Character_Type'Val (6312)),
   (Character_Type'Val (6314), Character_Type'Val (6314)),
   (Character_Type'Val (6320), Character_Type'Val (6389)),
   (Character_Type'Val (6400), Character_Type'Val (6428)),
   (Character_Type'Val (6480), Character_Type'Val (6509)),
   (Character_Type'Val (6512), Character_Type'Val (6516)),
   (Character_Type'Val (6528), Character_Type'Val (6571)),
   (Character_Type'Val (6593), Character_Type'Val (6599)),
   (Character_Type'Val (6656), Character_Type'Val (6678)),
   (Character_Type'Val (6688), Character_Type'Val (6740)),
   (Character_Type'Val (6917), Character_Type'Val (6963)),
   (Character_Type'Val (6981), Character_Type'Val (6987)),
   (Character_Type'Val (7043), Character_Type'Val (7072)),
   (Character_Type'Val (7086), Character_Type'Val (7087)),
   (Character_Type'Val (7168), Character_Type'Val (7203)),
   (Character_Type'Val (7245), Character_Type'Val (7247)),
   (Character_Type'Val (7258), Character_Type'Val (7287)),
   (Character_Type'Val (7401), Character_Type'Val (7404)),
   (Character_Type'Val (7406), Character_Type'Val (7409)),
   (Character_Type'Val (8501), Character_Type'Val (8504)),
   (Character_Type'Val (11568), Character_Type'Val (11621)),
   (Character_Type'Val (11648), Character_Type'Val (11670)),
   (Character_Type'Val (11680), Character_Type'Val (11686)),
   (Character_Type'Val (11688), Character_Type'Val (11694)),
   (Character_Type'Val (11696), Character_Type'Val (11702)),
   (Character_Type'Val (11704), Character_Type'Val (11710)),
   (Character_Type'Val (11712), Character_Type'Val (11718)),
   (Character_Type'Val (11720), Character_Type'Val (11726)),
   (Character_Type'Val (11728), Character_Type'Val (11734)),
   (Character_Type'Val (11736), Character_Type'Val (11742)),
   (Character_Type'Val (12294), Character_Type'Val (12294)),
   (Character_Type'Val (12348), Character_Type'Val (12348)),
   (Character_Type'Val (12353), Character_Type'Val (12438)),
   (Character_Type'Val (12447), Character_Type'Val (12447)),
   (Character_Type'Val (12449), Character_Type'Val (12538)),
   (Character_Type'Val (12543), Character_Type'Val (12543)),
   (Character_Type'Val (12549), Character_Type'Val (12589)),
   (Character_Type'Val (12593), Character_Type'Val (12686)),
   (Character_Type'Val (12704), Character_Type'Val (12727)),
   (Character_Type'Val (12784), Character_Type'Val (12799)),
   (Character_Type'Val (13312), Character_Type'Val (19893)),
   (Character_Type'Val (19968), Character_Type'Val (40907)),
   (Character_Type'Val (40960), Character_Type'Val (40980)),
   (Character_Type'Val (40982), Character_Type'Val (42124)),
   (Character_Type'Val (42192), Character_Type'Val (42231)),
   (Character_Type'Val (42240), Character_Type'Val (42507)),
   (Character_Type'Val (42512), Character_Type'Val (42527)),
   (Character_Type'Val (42538), Character_Type'Val (42539)),
   (Character_Type'Val (42606), Character_Type'Val (42606)),
   (Character_Type'Val (42656), Character_Type'Val (42725)),
   (Character_Type'Val (43003), Character_Type'Val (43009)),
   (Character_Type'Val (43011), Character_Type'Val (43013)),
   (Character_Type'Val (43015), Character_Type'Val (43018)),
   (Character_Type'Val (43020), Character_Type'Val (43042)),
   (Character_Type'Val (43072), Character_Type'Val (43123)),
   (Character_Type'Val (43138), Character_Type'Val (43187)),
   (Character_Type'Val (43250), Character_Type'Val (43255)),
   (Character_Type'Val (43259), Character_Type'Val (43259)),
   (Character_Type'Val (43274), Character_Type'Val (43301)),
   (Character_Type'Val (43312), Character_Type'Val (43334)),
   (Character_Type'Val (43360), Character_Type'Val (43388)),
   (Character_Type'Val (43396), Character_Type'Val (43442)),
   (Character_Type'Val (43520), Character_Type'Val (43560)),
   (Character_Type'Val (43584), Character_Type'Val (43586)),
   (Character_Type'Val (43588), Character_Type'Val (43595)),
   (Character_Type'Val (43616), Character_Type'Val (43631)),
   (Character_Type'Val (43633), Character_Type'Val (43638)),
   (Character_Type'Val (43642), Character_Type'Val (43642)),
   (Character_Type'Val (43648), Character_Type'Val (43695)),
   (Character_Type'Val (43697), Character_Type'Val (43697)),
   (Character_Type'Val (43701), Character_Type'Val (43702)),
   (Character_Type'Val (43705), Character_Type'Val (43709)),
   (Character_Type'Val (43712), Character_Type'Val (43712)),
   (Character_Type'Val (43714), Character_Type'Val (43714)),
   (Character_Type'Val (43739), Character_Type'Val (43740)),
   (Character_Type'Val (43968), Character_Type'Val (44002)),
   (Character_Type'Val (44032), Character_Type'Val (55203)),
   (Character_Type'Val (55216), Character_Type'Val (55238)),
   (Character_Type'Val (55243), Character_Type'Val (55291)),
   (Character_Type'Val (63744), Character_Type'Val (64045)),
   (Character_Type'Val (64048), Character_Type'Val (64109)),
   (Character_Type'Val (64112), Character_Type'Val (64217)),
   (Character_Type'Val (64285), Character_Type'Val (64285)),
   (Character_Type'Val (64287), Character_Type'Val (64296)),
   (Character_Type'Val (64298), Character_Type'Val (64310)),
   (Character_Type'Val (64312), Character_Type'Val (64316)),
   (Character_Type'Val (64318), Character_Type'Val (64318)),
   (Character_Type'Val (64320), Character_Type'Val (64321)),
   (Character_Type'Val (64323), Character_Type'Val (64324)),
   (Character_Type'Val (64326), Character_Type'Val (64433)),
   (Character_Type'Val (64467), Character_Type'Val (64829)),
   (Character_Type'Val (64848), Character_Type'Val (64911)),
   (Character_Type'Val (64914), Character_Type'Val (64967)),
   (Character_Type'Val (65008), Character_Type'Val (65019)),
   (Character_Type'Val (65136), Character_Type'Val (65140)),
   (Character_Type'Val (65142), Character_Type'Val (65276)),
   (Character_Type'Val (65382), Character_Type'Val (65391)),
   (Character_Type'Val (65393), Character_Type'Val (65437)),
   (Character_Type'Val (65440), Character_Type'Val (65470)),
   (Character_Type'Val (65474), Character_Type'Val (65479)),
   (Character_Type'Val (65482), Character_Type'Val (65487)),
   (Character_Type'Val (65490), Character_Type'Val (65495)),
   (Character_Type'Val (65498), Character_Type'Val (65500)),
   (Character_Type'Val (65536), Character_Type'Val (65547)),
   (Character_Type'Val (65549), Character_Type'Val (65574)),
   (Character_Type'Val (65576), Character_Type'Val (65594)),
   (Character_Type'Val (65596), Character_Type'Val (65597)),
   (Character_Type'Val (65599), Character_Type'Val (65613)),
   (Character_Type'Val (65616), Character_Type'Val (65629)),
   (Character_Type'Val (65664), Character_Type'Val (65786)),
   (Character_Type'Val (66176), Character_Type'Val (66204)),
   (Character_Type'Val (66208), Character_Type'Val (66256)),
   (Character_Type'Val (66304), Character_Type'Val (66334)),
   (Character_Type'Val (66352), Character_Type'Val (66368)),
   (Character_Type'Val (66370), Character_Type'Val (66377)),
   (Character_Type'Val (66432), Character_Type'Val (66461)),
   (Character_Type'Val (66464), Character_Type'Val (66499)),
   (Character_Type'Val (66504), Character_Type'Val (66511)),
   (Character_Type'Val (66640), Character_Type'Val (66717)),
   (Character_Type'Val (67584), Character_Type'Val (67589)),
   (Character_Type'Val (67592), Character_Type'Val (67592)),
   (Character_Type'Val (67594), Character_Type'Val (67637)),
   (Character_Type'Val (67639), Character_Type'Val (67640)),
   (Character_Type'Val (67644), Character_Type'Val (67644)),
   (Character_Type'Val (67647), Character_Type'Val (67669)),
   (Character_Type'Val (67840), Character_Type'Val (67861)),
   (Character_Type'Val (67872), Character_Type'Val (67897)),
   (Character_Type'Val (68096), Character_Type'Val (68096)),
   (Character_Type'Val (68112), Character_Type'Val (68115)),
   (Character_Type'Val (68117), Character_Type'Val (68119)),
   (Character_Type'Val (68121), Character_Type'Val (68147)),
   (Character_Type'Val (68192), Character_Type'Val (68220)),
   (Character_Type'Val (68352), Character_Type'Val (68405)),
   (Character_Type'Val (68416), Character_Type'Val (68437)),
   (Character_Type'Val (68448), Character_Type'Val (68466)),
   (Character_Type'Val (68608), Character_Type'Val (68680)),
   (Character_Type'Val (69763), Character_Type'Val (69807)),
   (Character_Type'Val (73728), Character_Type'Val (74606)),
   (Character_Type'Val (77824), Character_Type'Val (78894)),
   (Character_Type'Val (131072), Character_Type'Val (173782)),
   (Character_Type'Val (173824), Character_Type'Val (177972)),
   (Character_Type'Val (194560), Character_Type'Val (195101))
   );
   Ranges_2 : constant Character_Range_Array := (
   (Character_Type'Val (688), Character_Type'Val (705)),
   (Character_Type'Val (710), Character_Type'Val (721)),
   (Character_Type'Val (736), Character_Type'Val (740)),
   (Character_Type'Val (748), Character_Type'Val (748)),
   (Character_Type'Val (750), Character_Type'Val (750)),
   (Character_Type'Val (884), Character_Type'Val (884)),
   (Character_Type'Val (890), Character_Type'Val (890)),
   (Character_Type'Val (1369), Character_Type'Val (1369)),
   (Character_Type'Val (1600), Character_Type'Val (1600)),
   (Character_Type'Val (1765), Character_Type'Val (1766)),
   (Character_Type'Val (2036), Character_Type'Val (2037)),
   (Character_Type'Val (2042), Character_Type'Val (2042)),
   (Character_Type'Val (2074), Character_Type'Val (2074)),
   (Character_Type'Val (2084), Character_Type'Val (2084)),
   (Character_Type'Val (2088), Character_Type'Val (2088)),
   (Character_Type'Val (2417), Character_Type'Val (2417)),
   (Character_Type'Val (3654), Character_Type'Val (3654)),
   (Character_Type'Val (3782), Character_Type'Val (3782)),
   (Character_Type'Val (4348), Character_Type'Val (4348)),
   (Character_Type'Val (6103), Character_Type'Val (6103)),
   (Character_Type'Val (6211), Character_Type'Val (6211)),
   (Character_Type'Val (6823), Character_Type'Val (6823)),
   (Character_Type'Val (7288), Character_Type'Val (7293)),
   (Character_Type'Val (7468), Character_Type'Val (7521)),
   (Character_Type'Val (7544), Character_Type'Val (7544)),
   (Character_Type'Val (7579), Character_Type'Val (7615)),
   (Character_Type'Val (8305), Character_Type'Val (8305)),
   (Character_Type'Val (8319), Character_Type'Val (8319)),
   (Character_Type'Val (8336), Character_Type'Val (8340)),
   (Character_Type'Val (11389), Character_Type'Val (11389)),
   (Character_Type'Val (11631), Character_Type'Val (11631)),
   (Character_Type'Val (11823), Character_Type'Val (11823)),
   (Character_Type'Val (12293), Character_Type'Val (12293)),
   (Character_Type'Val (12337), Character_Type'Val (12341)),
   (Character_Type'Val (12347), Character_Type'Val (12347)),
   (Character_Type'Val (12445), Character_Type'Val (12446)),
   (Character_Type'Val (12540), Character_Type'Val (12542)),
   (Character_Type'Val (40981), Character_Type'Val (40981)),
   (Character_Type'Val (42232), Character_Type'Val (42237)),
   (Character_Type'Val (42508), Character_Type'Val (42508)),
   (Character_Type'Val (42623), Character_Type'Val (42623)),
   (Character_Type'Val (42775), Character_Type'Val (42783)),
   (Character_Type'Val (42864), Character_Type'Val (42864)),
   (Character_Type'Val (42888), Character_Type'Val (42888)),
   (Character_Type'Val (43471), Character_Type'Val (43471)),
   (Character_Type'Val (43632), Character_Type'Val (43632)),
   (Character_Type'Val (43741), Character_Type'Val (43741)),
   (Character_Type'Val (65392), Character_Type'Val (65392)),
   (Character_Type'Val (65438), Character_Type'Val (65439))
   );
   Ranges_3 : constant Character_Range_Array := (
   (Character_Type'Val (5870), Character_Type'Val (5872)),
   (Character_Type'Val (8544), Character_Type'Val (8578)),
   (Character_Type'Val (8581), Character_Type'Val (8584)),
   (Character_Type'Val (12295), Character_Type'Val (12295)),
   (Character_Type'Val (12321), Character_Type'Val (12329)),
   (Character_Type'Val (12344), Character_Type'Val (12346)),
   (Character_Type'Val (42726), Character_Type'Val (42735)),
   (Character_Type'Val (65856), Character_Type'Val (65908)),
   (Character_Type'Val (66369), Character_Type'Val (66369)),
   (Character_Type'Val (66378), Character_Type'Val (66378)),
   (Character_Type'Val (66513), Character_Type'Val (66517)),
   (Character_Type'Val (74752), Character_Type'Val (74850))
   );
   Ranges_4 : constant Character_Range_Array := (
   (Character_Type'Val (1632), Character_Type'Val (1641)),
   (Character_Type'Val (1776), Character_Type'Val (1785)),
   (Character_Type'Val (1984), Character_Type'Val (1993)),
   (Character_Type'Val (2406), Character_Type'Val (2415)),
   (Character_Type'Val (2534), Character_Type'Val (2543)),
   (Character_Type'Val (2662), Character_Type'Val (2671)),
   (Character_Type'Val (2790), Character_Type'Val (2799)),
   (Character_Type'Val (2918), Character_Type'Val (2927)),
   (Character_Type'Val (3046), Character_Type'Val (3055)),
   (Character_Type'Val (3174), Character_Type'Val (3183)),
   (Character_Type'Val (3302), Character_Type'Val (3311)),
   (Character_Type'Val (3430), Character_Type'Val (3439)),
   (Character_Type'Val (3664), Character_Type'Val (3673)),
   (Character_Type'Val (3792), Character_Type'Val (3801)),
   (Character_Type'Val (3872), Character_Type'Val (3881)),
   (Character_Type'Val (4160), Character_Type'Val (4169)),
   (Character_Type'Val (4240), Character_Type'Val (4249)),
   (Character_Type'Val (6112), Character_Type'Val (6121)),
   (Character_Type'Val (6160), Character_Type'Val (6169)),
   (Character_Type'Val (6470), Character_Type'Val (6479)),
   (Character_Type'Val (6608), Character_Type'Val (6618)),
   (Character_Type'Val (6784), Character_Type'Val (6793)),
   (Character_Type'Val (6800), Character_Type'Val (6809)),
   (Character_Type'Val (6992), Character_Type'Val (7001)),
   (Character_Type'Val (7088), Character_Type'Val (7097)),
   (Character_Type'Val (7232), Character_Type'Val (7241)),
   (Character_Type'Val (7248), Character_Type'Val (7257)),
   (Character_Type'Val (42528), Character_Type'Val (42537)),
   (Character_Type'Val (43216), Character_Type'Val (43225)),
   (Character_Type'Val (43264), Character_Type'Val (43273)),
   (Character_Type'Val (43472), Character_Type'Val (43481)),
   (Character_Type'Val (43600), Character_Type'Val (43609)),
   (Character_Type'Val (44016), Character_Type'Val (44025)),
   (Character_Type'Val (65296), Character_Type'Val (65305)),
   (Character_Type'Val (66720), Character_Type'Val (66729)),
   (Character_Type'Val (120782), Character_Type'Val (120831))
   );
   Ranges_5 : constant Character_Range_Array := (
   (Character_Type'Val (768), Character_Type'Val (879)),
   (Character_Type'Val (1155), Character_Type'Val (1159)),
   (Character_Type'Val (1425), Character_Type'Val (1469)),
   (Character_Type'Val (1471), Character_Type'Val (1471)),
   (Character_Type'Val (1473), Character_Type'Val (1474)),
   (Character_Type'Val (1476), Character_Type'Val (1477)),
   (Character_Type'Val (1479), Character_Type'Val (1479)),
   (Character_Type'Val (1552), Character_Type'Val (1562)),
   (Character_Type'Val (1611), Character_Type'Val (1630)),
   (Character_Type'Val (1648), Character_Type'Val (1648)),
   (Character_Type'Val (1750), Character_Type'Val (1756)),
   (Character_Type'Val (1759), Character_Type'Val (1764)),
   (Character_Type'Val (1767), Character_Type'Val (1768)),
   (Character_Type'Val (1770), Character_Type'Val (1773)),
   (Character_Type'Val (1809), Character_Type'Val (1809)),
   (Character_Type'Val (1840), Character_Type'Val (1866)),
   (Character_Type'Val (1958), Character_Type'Val (1968)),
   (Character_Type'Val (2027), Character_Type'Val (2035)),
   (Character_Type'Val (2070), Character_Type'Val (2073)),
   (Character_Type'Val (2075), Character_Type'Val (2083)),
   (Character_Type'Val (2085), Character_Type'Val (2087)),
   (Character_Type'Val (2089), Character_Type'Val (2093)),
   (Character_Type'Val (2304), Character_Type'Val (2306)),
   (Character_Type'Val (2364), Character_Type'Val (2364)),
   (Character_Type'Val (2369), Character_Type'Val (2376)),
   (Character_Type'Val (2381), Character_Type'Val (2381)),
   (Character_Type'Val (2385), Character_Type'Val (2389)),
   (Character_Type'Val (2402), Character_Type'Val (2403)),
   (Character_Type'Val (2433), Character_Type'Val (2433)),
   (Character_Type'Val (2492), Character_Type'Val (2492)),
   (Character_Type'Val (2497), Character_Type'Val (2500)),
   (Character_Type'Val (2509), Character_Type'Val (2509)),
   (Character_Type'Val (2530), Character_Type'Val (2531)),
   (Character_Type'Val (2561), Character_Type'Val (2562)),
   (Character_Type'Val (2620), Character_Type'Val (2620)),
   (Character_Type'Val (2625), Character_Type'Val (2626)),
   (Character_Type'Val (2631), Character_Type'Val (2632)),
   (Character_Type'Val (2635), Character_Type'Val (2637)),
   (Character_Type'Val (2641), Character_Type'Val (2641)),
   (Character_Type'Val (2672), Character_Type'Val (2673)),
   (Character_Type'Val (2677), Character_Type'Val (2677)),
   (Character_Type'Val (2689), Character_Type'Val (2690)),
   (Character_Type'Val (2748), Character_Type'Val (2748)),
   (Character_Type'Val (2753), Character_Type'Val (2757)),
   (Character_Type'Val (2759), Character_Type'Val (2760)),
   (Character_Type'Val (2765), Character_Type'Val (2765)),
   (Character_Type'Val (2786), Character_Type'Val (2787)),
   (Character_Type'Val (2817), Character_Type'Val (2817)),
   (Character_Type'Val (2876), Character_Type'Val (2876)),
   (Character_Type'Val (2879), Character_Type'Val (2879)),
   (Character_Type'Val (2881), Character_Type'Val (2884)),
   (Character_Type'Val (2893), Character_Type'Val (2893)),
   (Character_Type'Val (2902), Character_Type'Val (2902)),
   (Character_Type'Val (2914), Character_Type'Val (2915)),
   (Character_Type'Val (2946), Character_Type'Val (2946)),
   (Character_Type'Val (3008), Character_Type'Val (3008)),
   (Character_Type'Val (3021), Character_Type'Val (3021)),
   (Character_Type'Val (3134), Character_Type'Val (3136)),
   (Character_Type'Val (3142), Character_Type'Val (3144)),
   (Character_Type'Val (3146), Character_Type'Val (3149)),
   (Character_Type'Val (3157), Character_Type'Val (3158)),
   (Character_Type'Val (3170), Character_Type'Val (3171)),
   (Character_Type'Val (3260), Character_Type'Val (3260)),
   (Character_Type'Val (3263), Character_Type'Val (3263)),
   (Character_Type'Val (3270), Character_Type'Val (3270)),
   (Character_Type'Val (3276), Character_Type'Val (3277)),
   (Character_Type'Val (3298), Character_Type'Val (3299)),
   (Character_Type'Val (3393), Character_Type'Val (3396)),
   (Character_Type'Val (3405), Character_Type'Val (3405)),
   (Character_Type'Val (3426), Character_Type'Val (3427)),
   (Character_Type'Val (3530), Character_Type'Val (3530)),
   (Character_Type'Val (3538), Character_Type'Val (3540)),
   (Character_Type'Val (3542), Character_Type'Val (3542)),
   (Character_Type'Val (3633), Character_Type'Val (3633)),
   (Character_Type'Val (3636), Character_Type'Val (3642)),
   (Character_Type'Val (3655), Character_Type'Val (3662)),
   (Character_Type'Val (3761), Character_Type'Val (3761)),
   (Character_Type'Val (3764), Character_Type'Val (3769)),
   (Character_Type'Val (3771), Character_Type'Val (3772)),
   (Character_Type'Val (3784), Character_Type'Val (3789)),
   (Character_Type'Val (3864), Character_Type'Val (3865)),
   (Character_Type'Val (3893), Character_Type'Val (3893)),
   (Character_Type'Val (3895), Character_Type'Val (3895)),
   (Character_Type'Val (3897), Character_Type'Val (3897)),
   (Character_Type'Val (3953), Character_Type'Val (3966)),
   (Character_Type'Val (3968), Character_Type'Val (3972)),
   (Character_Type'Val (3974), Character_Type'Val (3975)),
   (Character_Type'Val (3984), Character_Type'Val (3991)),
   (Character_Type'Val (3993), Character_Type'Val (4028)),
   (Character_Type'Val (4038), Character_Type'Val (4038)),
   (Character_Type'Val (4141), Character_Type'Val (4144)),
   (Character_Type'Val (4146), Character_Type'Val (4151)),
   (Character_Type'Val (4153), Character_Type'Val (4154)),
   (Character_Type'Val (4157), Character_Type'Val (4158)),
   (Character_Type'Val (4184), Character_Type'Val (4185)),
   (Character_Type'Val (4190), Character_Type'Val (4192)),
   (Character_Type'Val (4209), Character_Type'Val (4212)),
   (Character_Type'Val (4226), Character_Type'Val (4226)),
   (Character_Type'Val (4229), Character_Type'Val (4230)),
   (Character_Type'Val (4237), Character_Type'Val (4237)),
   (Character_Type'Val (4253), Character_Type'Val (4253)),
   (Character_Type'Val (4959), Character_Type'Val (4959)),
   (Character_Type'Val (5906), Character_Type'Val (5908)),
   (Character_Type'Val (5938), Character_Type'Val (5940)),
   (Character_Type'Val (5970), Character_Type'Val (5971)),
   (Character_Type'Val (6002), Character_Type'Val (6003)),
   (Character_Type'Val (6071), Character_Type'Val (6077)),
   (Character_Type'Val (6086), Character_Type'Val (6086)),
   (Character_Type'Val (6089), Character_Type'Val (6099)),
   (Character_Type'Val (6109), Character_Type'Val (6109)),
   (Character_Type'Val (6155), Character_Type'Val (6157)),
   (Character_Type'Val (6313), Character_Type'Val (6313)),
   (Character_Type'Val (6432), Character_Type'Val (6434)),
   (Character_Type'Val (6439), Character_Type'Val (6440)),
   (Character_Type'Val (6450), Character_Type'Val (6450)),
   (Character_Type'Val (6457), Character_Type'Val (6459)),
   (Character_Type'Val (6679), Character_Type'Val (6680)),
   (Character_Type'Val (6742), Character_Type'Val (6742)),
   (Character_Type'Val (6744), Character_Type'Val (6750)),
   (Character_Type'Val (6752), Character_Type'Val (6752)),
   (Character_Type'Val (6754), Character_Type'Val (6754)),
   (Character_Type'Val (6757), Character_Type'Val (6764)),
   (Character_Type'Val (6771), Character_Type'Val (6780)),
   (Character_Type'Val (6783), Character_Type'Val (6783)),
   (Character_Type'Val (6912), Character_Type'Val (6915)),
   (Character_Type'Val (6964), Character_Type'Val (6964)),
   (Character_Type'Val (6966), Character_Type'Val (6970)),
   (Character_Type'Val (6972), Character_Type'Val (6972)),
   (Character_Type'Val (6978), Character_Type'Val (6978)),
   (Character_Type'Val (7019), Character_Type'Val (7027)),
   (Character_Type'Val (7040), Character_Type'Val (7041)),
   (Character_Type'Val (7074), Character_Type'Val (7077)),
   (Character_Type'Val (7080), Character_Type'Val (7081)),
   (Character_Type'Val (7212), Character_Type'Val (7219)),
   (Character_Type'Val (7222), Character_Type'Val (7223)),
   (Character_Type'Val (7376), Character_Type'Val (7378)),
   (Character_Type'Val (7380), Character_Type'Val (7392)),
   (Character_Type'Val (7394), Character_Type'Val (7400)),
   (Character_Type'Val (7405), Character_Type'Val (7405)),
   (Character_Type'Val (7616), Character_Type'Val (7654)),
   (Character_Type'Val (7677), Character_Type'Val (7679)),
   (Character_Type'Val (8400), Character_Type'Val (8412)),
   (Character_Type'Val (8417), Character_Type'Val (8417)),
   (Character_Type'Val (8421), Character_Type'Val (8432)),
   (Character_Type'Val (11503), Character_Type'Val (11505)),
   (Character_Type'Val (11744), Character_Type'Val (11775)),
   (Character_Type'Val (12330), Character_Type'Val (12335)),
   (Character_Type'Val (12441), Character_Type'Val (12442)),
   (Character_Type'Val (42607), Character_Type'Val (42607)),
   (Character_Type'Val (42620), Character_Type'Val (42621)),
   (Character_Type'Val (42736), Character_Type'Val (42737)),
   (Character_Type'Val (43010), Character_Type'Val (43010)),
   (Character_Type'Val (43014), Character_Type'Val (43014)),
   (Character_Type'Val (43019), Character_Type'Val (43019)),
   (Character_Type'Val (43045), Character_Type'Val (43046)),
   (Character_Type'Val (43204), Character_Type'Val (43204)),
   (Character_Type'Val (43232), Character_Type'Val (43249)),
   (Character_Type'Val (43302), Character_Type'Val (43309)),
   (Character_Type'Val (43335), Character_Type'Val (43345)),
   (Character_Type'Val (43392), Character_Type'Val (43394)),
   (Character_Type'Val (43443), Character_Type'Val (43443)),
   (Character_Type'Val (43446), Character_Type'Val (43449)),
   (Character_Type'Val (43452), Character_Type'Val (43452)),
   (Character_Type'Val (43561), Character_Type'Val (43566)),
   (Character_Type'Val (43569), Character_Type'Val (43570)),
   (Character_Type'Val (43573), Character_Type'Val (43574)),
   (Character_Type'Val (43587), Character_Type'Val (43587)),
   (Character_Type'Val (43596), Character_Type'Val (43596)),
   (Character_Type'Val (43696), Character_Type'Val (43696)),
   (Character_Type'Val (43698), Character_Type'Val (43700)),
   (Character_Type'Val (43703), Character_Type'Val (43704)),
   (Character_Type'Val (43710), Character_Type'Val (43711)),
   (Character_Type'Val (43713), Character_Type'Val (43713)),
   (Character_Type'Val (44005), Character_Type'Val (44005)),
   (Character_Type'Val (44008), Character_Type'Val (44008)),
   (Character_Type'Val (44013), Character_Type'Val (44013)),
   (Character_Type'Val (64286), Character_Type'Val (64286)),
   (Character_Type'Val (65024), Character_Type'Val (65039)),
   (Character_Type'Val (65056), Character_Type'Val (65062)),
   (Character_Type'Val (66045), Character_Type'Val (66045)),
   (Character_Type'Val (68097), Character_Type'Val (68099)),
   (Character_Type'Val (68101), Character_Type'Val (68102)),
   (Character_Type'Val (68108), Character_Type'Val (68111)),
   (Character_Type'Val (68152), Character_Type'Val (68154)),
   (Character_Type'Val (68159), Character_Type'Val (68159)),
   (Character_Type'Val (69760), Character_Type'Val (69761)),
   (Character_Type'Val (69811), Character_Type'Val (69814)),
   (Character_Type'Val (69817), Character_Type'Val (69818)),
   (Character_Type'Val (119143), Character_Type'Val (119145)),
   (Character_Type'Val (119163), Character_Type'Val (119170)),
   (Character_Type'Val (119173), Character_Type'Val (119179)),
   (Character_Type'Val (119210), Character_Type'Val (119213)),
   (Character_Type'Val (119362), Character_Type'Val (119364)),
   (Character_Type'Val (917760), Character_Type'Val (917999))
   );
   Ranges_6 : constant Character_Range_Array := (
   (Character_Type'Val (2307), Character_Type'Val (2307)),
   (Character_Type'Val (2366), Character_Type'Val (2368)),
   (Character_Type'Val (2377), Character_Type'Val (2380)),
   (Character_Type'Val (2382), Character_Type'Val (2382)),
   (Character_Type'Val (2434), Character_Type'Val (2435)),
   (Character_Type'Val (2494), Character_Type'Val (2496)),
   (Character_Type'Val (2503), Character_Type'Val (2504)),
   (Character_Type'Val (2507), Character_Type'Val (2508)),
   (Character_Type'Val (2519), Character_Type'Val (2519)),
   (Character_Type'Val (2563), Character_Type'Val (2563)),
   (Character_Type'Val (2622), Character_Type'Val (2624)),
   (Character_Type'Val (2691), Character_Type'Val (2691)),
   (Character_Type'Val (2750), Character_Type'Val (2752)),
   (Character_Type'Val (2761), Character_Type'Val (2761)),
   (Character_Type'Val (2763), Character_Type'Val (2764)),
   (Character_Type'Val (2818), Character_Type'Val (2819)),
   (Character_Type'Val (2878), Character_Type'Val (2878)),
   (Character_Type'Val (2880), Character_Type'Val (2880)),
   (Character_Type'Val (2887), Character_Type'Val (2888)),
   (Character_Type'Val (2891), Character_Type'Val (2892)),
   (Character_Type'Val (2903), Character_Type'Val (2903)),
   (Character_Type'Val (3006), Character_Type'Val (3007)),
   (Character_Type'Val (3009), Character_Type'Val (3010)),
   (Character_Type'Val (3014), Character_Type'Val (3016)),
   (Character_Type'Val (3018), Character_Type'Val (3020)),
   (Character_Type'Val (3031), Character_Type'Val (3031)),
   (Character_Type'Val (3073), Character_Type'Val (3075)),
   (Character_Type'Val (3137), Character_Type'Val (3140)),
   (Character_Type'Val (3202), Character_Type'Val (3203)),
   (Character_Type'Val (3262), Character_Type'Val (3262)),
   (Character_Type'Val (3264), Character_Type'Val (3268)),
   (Character_Type'Val (3271), Character_Type'Val (3272)),
   (Character_Type'Val (3274), Character_Type'Val (3275)),
   (Character_Type'Val (3285), Character_Type'Val (3286)),
   (Character_Type'Val (3330), Character_Type'Val (3331)),
   (Character_Type'Val (3390), Character_Type'Val (3392)),
   (Character_Type'Val (3398), Character_Type'Val (3400)),
   (Character_Type'Val (3402), Character_Type'Val (3404)),
   (Character_Type'Val (3415), Character_Type'Val (3415)),
   (Character_Type'Val (3458), Character_Type'Val (3459)),
   (Character_Type'Val (3535), Character_Type'Val (3537)),
   (Character_Type'Val (3544), Character_Type'Val (3551)),
   (Character_Type'Val (3570), Character_Type'Val (3571)),
   (Character_Type'Val (3902), Character_Type'Val (3903)),
   (Character_Type'Val (3967), Character_Type'Val (3967)),
   (Character_Type'Val (4139), Character_Type'Val (4140)),
   (Character_Type'Val (4145), Character_Type'Val (4145)),
   (Character_Type'Val (4152), Character_Type'Val (4152)),
   (Character_Type'Val (4155), Character_Type'Val (4156)),
   (Character_Type'Val (4182), Character_Type'Val (4183)),
   (Character_Type'Val (4194), Character_Type'Val (4196)),
   (Character_Type'Val (4199), Character_Type'Val (4205)),
   (Character_Type'Val (4227), Character_Type'Val (4228)),
   (Character_Type'Val (4231), Character_Type'Val (4236)),
   (Character_Type'Val (4239), Character_Type'Val (4239)),
   (Character_Type'Val (4250), Character_Type'Val (4252)),
   (Character_Type'Val (6070), Character_Type'Val (6070)),
   (Character_Type'Val (6078), Character_Type'Val (6085)),
   (Character_Type'Val (6087), Character_Type'Val (6088)),
   (Character_Type'Val (6435), Character_Type'Val (6438)),
   (Character_Type'Val (6441), Character_Type'Val (6443)),
   (Character_Type'Val (6448), Character_Type'Val (6449)),
   (Character_Type'Val (6451), Character_Type'Val (6456)),
   (Character_Type'Val (6576), Character_Type'Val (6592)),
   (Character_Type'Val (6600), Character_Type'Val (6601)),
   (Character_Type'Val (6681), Character_Type'Val (6683)),
   (Character_Type'Val (6741), Character_Type'Val (6741)),
   (Character_Type'Val (6743), Character_Type'Val (6743)),
   (Character_Type'Val (6753), Character_Type'Val (6753)),
   (Character_Type'Val (6755), Character_Type'Val (6756)),
   (Character_Type'Val (6765), Character_Type'Val (6770)),
   (Character_Type'Val (6916), Character_Type'Val (6916)),
   (Character_Type'Val (6965), Character_Type'Val (6965)),
   (Character_Type'Val (6971), Character_Type'Val (6971)),
   (Character_Type'Val (6973), Character_Type'Val (6977)),
   (Character_Type'Val (6979), Character_Type'Val (6980)),
   (Character_Type'Val (7042), Character_Type'Val (7042)),
   (Character_Type'Val (7073), Character_Type'Val (7073)),
   (Character_Type'Val (7078), Character_Type'Val (7079)),
   (Character_Type'Val (7082), Character_Type'Val (7082)),
   (Character_Type'Val (7204), Character_Type'Val (7211)),
   (Character_Type'Val (7220), Character_Type'Val (7221)),
   (Character_Type'Val (7393), Character_Type'Val (7393)),
   (Character_Type'Val (7410), Character_Type'Val (7410)),
   (Character_Type'Val (43043), Character_Type'Val (43044)),
   (Character_Type'Val (43047), Character_Type'Val (43047)),
   (Character_Type'Val (43136), Character_Type'Val (43137)),
   (Character_Type'Val (43188), Character_Type'Val (43203)),
   (Character_Type'Val (43346), Character_Type'Val (43347)),
   (Character_Type'Val (43395), Character_Type'Val (43395)),
   (Character_Type'Val (43444), Character_Type'Val (43445)),
   (Character_Type'Val (43450), Character_Type'Val (43451)),
   (Character_Type'Val (43453), Character_Type'Val (43456)),
   (Character_Type'Val (43567), Character_Type'Val (43568)),
   (Character_Type'Val (43571), Character_Type'Val (43572)),
   (Character_Type'Val (43597), Character_Type'Val (43597)),
   (Character_Type'Val (43643), Character_Type'Val (43643)),
   (Character_Type'Val (44003), Character_Type'Val (44004)),
   (Character_Type'Val (44006), Character_Type'Val (44007)),
   (Character_Type'Val (44009), Character_Type'Val (44010)),
   (Character_Type'Val (44012), Character_Type'Val (44012)),
   (Character_Type'Val (69762), Character_Type'Val (69762)),
   (Character_Type'Val (69808), Character_Type'Val (69810)),
   (Character_Type'Val (69815), Character_Type'Val (69816)),
   (Character_Type'Val (119141), Character_Type'Val (119142)),
   (Character_Type'Val (119149), Character_Type'Val (119154))
   );

   ----------------
   -- Next_Token --
   ----------------

   procedure Next_Token
     (Self : in out Lexer_State; Token : out Lexed_Token)
   is
      Input : constant Text_Access := Self.Input;

      First_Index : Positive;
      --  Index of the first input character for the token to return

      Index : Positive;
      --  Index for the next input character to be analyzed

      Match_Index : Natural;
      --  If we found a match, index for its last character. Otherwise, zero.

      Match_Ignore : Boolean;
      --  If we found a match, whether we must ignore it and restart the
      --  automaton after its character range.

      Match_Kind : Token_Kind;
      --  If we found a match and it is not ignored, kind for the token to
      --  emit. Meaningless otherwise.
   begin
      First_Index := Self.Last_Token.Text_Last + 1;

      <<Start>>
      Index := First_Index;
      Match_Index := 0;
      Match_Ignore := False;



         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#9#) .. Character_Type'Val (16#d#) | ' ' => goto State_1;
               when '!' => goto State_2;
               when '"' => goto State_3;
               when '#' => goto State_4;
               when '$' | '_' => goto State_5;
               when '%' => goto State_6;
               when '&' => goto State_7;
               when ''' => goto State_8;
               when '(' => goto State_9;
               when ')' => goto State_10;
               when '*' => goto State_11;
               when '+' => goto State_12;
               when ',' => goto State_13;
               when '-' => goto State_14;
               when '.' => goto State_15;
               when '/' => goto State_16;
               when '0' .. '9' => goto State_17;
               when ':' => goto State_18;
               when ';' => goto State_19;
               when '<' => goto State_20;
               when '=' => goto State_21;
               when '>' => goto State_22;
               when '@' => goto State_23;
               when 'A' | 'a' => goto State_24;
               when 'B' .. 'A' | 'C' .. 'B' | 'D' .. 'C' | 'E' .. 'D' | 'F' .. 'E' | 'G' .. 'F' | 'H' | 'J' .. 'K' | 'M' .. 'L' | 'N' .. 'M' | 'O' .. 'N' | 'P' .. 'O' | 'Q' | 'S' .. 'R' | 'T' .. 'S' | 'U' .. 'T' | 'V' | 'X' .. 'W' | 'Y' .. 'Z' | 'b' .. 'a' | 'c' .. 'b' | 'd' .. 'c' | 'e' .. 'd' | 'f' .. 'e' | 'g' .. 'f' | 'h' | 'j' .. 'k' | 'm' .. 'l' | 'n' .. 'm' | 'o' .. 'n' | 'p' .. 'o' | 'q' | 's' .. 'r' | 't' .. 's' | 'u' .. 't' | 'v' | 'x' .. 'w' | 'y' .. 'z' => goto State_25;
               when 'B' | 'b' => goto State_26;
               when 'C' | 'c' => goto State_27;
               when 'D' | 'd' => goto State_28;
               when 'E' | 'e' => goto State_29;
               when 'F' | 'f' => goto State_30;
               when 'G' | 'g' => goto State_31;
               when 'I' | 'i' => goto State_32;
               when 'L' | 'l' => goto State_33;
               when 'M' | 'm' => goto State_34;
               when 'N' | 'n' => goto State_35;
               when 'O' | 'o' => goto State_36;
               when 'P' | 'p' => goto State_37;
               when 'R' | 'r' => goto State_38;
               when 'S' | 's' => goto State_39;
               when 'T' | 't' => goto State_40;
               when 'U' | 'u' => goto State_41;
               when 'W' | 'w' => goto State_42;
               when 'X' | 'x' => goto State_43;
               when '[' => goto State_44;
               when ']' => goto State_45;
               when '|' => goto State_46;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_0) then
                        goto State_25;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_47;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_48;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_49;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_1>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Whitespace;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#9#) .. Character_Type'Val (16#d#) | ' ' => goto State_50;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_2>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Pipe;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_3>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. 'Z' | '\' .. Character_Type'Val (16#10ffff#) => goto State_51;
               when '"' => goto State_52;
               when '[' => goto State_53;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_4>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Prep_Line;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#10ffff#) => goto State_54;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_5>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_25;
               when '[' => goto State_55;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_0) then
                        goto State_25;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_47;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_48;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_49;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_6>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '$' | '&' .. 'Z' | '\' .. Character_Type'Val (16#10ffff#) => goto State_56;
               when '%' => goto State_57;
               when '[' => goto State_58;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_7>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Amp;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_8>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Tick;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. 'Z' | '\' .. Character_Type'Val (16#10ffff#) => goto State_59;
               when '[' => goto State_60;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_9>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Par_Open;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_10>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Par_Close;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_11>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Mult;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '*' => goto State_61;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_12>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Plus;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_13>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Comma;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_14>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Minus;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '-' => goto State_62;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_15>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Dot;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '.' => goto State_63;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_16>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Divide;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_64;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_17>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Integer;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '#' | ':' => goto State_65;
               when '.' => goto State_66;
               when '0' .. '9' => goto State_67;
               when 'E' | 'e' => goto State_68;
               when '_' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_18>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Colon;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_70;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_19>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Semicolon;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_20>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Lt;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '<' => goto State_71;
               when '=' => goto State_72;
               when '>' => goto State_73;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_21>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Equal;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '>' => goto State_74;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_22>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Gt;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_75;
               when '>' => goto State_76;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_23>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Target;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_24>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'C' .. 'B' | 'D' .. 'K' | 'M' | 'O' .. 'Q' | 'S' | 'U' .. 'Z' | 'a' | 'c' .. 'b' | 'd' .. 'k' | 'm' | 'o' .. 'q' | 's' | 'u' .. 'z' => goto State_78;
               when 'B' | 'b' => goto State_79;
               when 'C' | 'c' => goto State_80;
               when 'L' | 'l' => goto State_81;
               when 'N' | 'n' => goto State_82;
               when 'R' | 'r' => goto State_83;
               when 'T' | 't' => goto State_84;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_25>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_26>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'N' | 'P' .. 'Z' | 'a' .. 'd' | 'f' .. 'n' | 'p' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_92;
               when 'O' | 'o' => goto State_93;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_27>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_94;
               when 'B' .. 'N' | 'P' .. 'Z' | 'b' .. 'n' | 'p' .. 'z' => goto State_78;
               when 'O' | 'o' => goto State_95;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_28>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'H' | 'J' .. 'N' | 'P' .. 'Z' | 'a' .. 'd' | 'f' .. 'h' | 'j' .. 'n' | 'p' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_96;
               when 'I' | 'i' => goto State_97;
               when 'O' | 'o' => goto State_98;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_29>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'K' | 'M' | 'O' .. 'W' | 'Y' .. 'Z' | 'a' .. 'k' | 'm' | 'o' .. 'w' | 'y' .. 'z' => goto State_78;
               when 'L' | 'l' => goto State_99;
               when 'N' | 'n' => goto State_100;
               when 'X' | 'x' => goto State_101;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_30>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'N' | 'P' .. 'T' | 'V' .. 'Z' | 'a' .. 'n' | 'p' .. 't' | 'v' .. 'z' => goto State_78;
               when 'O' | 'o' => goto State_102;
               when 'U' | 'u' => goto State_103;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_31>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'N' | 'P' .. 'Z' | 'a' .. 'd' | 'f' .. 'n' | 'p' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_104;
               when 'O' | 'o' => goto State_105;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_32>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'E' | 'G' .. 'M' | 'O' .. 'R' | 'T' .. 'Z' | 'a' .. 'e' | 'g' .. 'm' | 'o' .. 'r' | 't' .. 'z' => goto State_78;
               when 'F' | 'f' => goto State_106;
               when 'N' | 'n' => goto State_107;
               when 'S' | 's' => goto State_108;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_33>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'H' | 'J' .. 'N' | 'P' .. 'Z' | 'a' .. 'h' | 'j' .. 'n' | 'p' .. 'z' => goto State_78;
               when 'I' | 'i' => goto State_109;
               when 'O' | 'o' => goto State_110;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_34>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'N' | 'P' .. 'Z' | 'a' .. 'n' | 'p' .. 'z' => goto State_78;
               when 'O' | 'o' => goto State_111;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_35>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'N' | 'P' .. 'T' | 'V' .. 'Z' | 'a' .. 'd' | 'f' .. 'n' | 'p' .. 't' | 'v' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_112;
               when 'O' | 'o' => goto State_113;
               when 'U' | 'u' => goto State_114;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_36>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'E' | 'G' .. 'Q' | 'S' | 'U' .. 'T' | 'V' .. 'Z' | 'a' .. 'e' | 'g' .. 'q' | 's' | 'u' .. 't' | 'v' .. 'z' => goto State_78;
               when 'F' | 'f' => goto State_115;
               when 'R' | 'r' => goto State_116;
               when 'T' | 't' => goto State_117;
               when 'U' | 'u' => goto State_118;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_37>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_119;
               when 'B' .. 'Q' | 'S' .. 'Z' | 'b' .. 'q' | 's' .. 'z' => goto State_78;
               when 'R' | 'r' => goto State_120;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_38>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_121;
               when 'B' .. 'D' | 'F' .. 'Z' | 'b' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_122;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_39>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'T' | 'V' .. 'Z' | 'a' .. 'd' | 'f' .. 't' | 'v' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_123;
               when 'U' | 'u' => goto State_124;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_40>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_125;
               when 'B' .. 'D' | 'F' .. 'G' | 'I' .. 'X' | 'Z' | 'b' .. 'd' | 'f' .. 'g' | 'i' .. 'x' | 'z' => goto State_78;
               when 'E' | 'e' => goto State_126;
               when 'H' | 'h' => goto State_127;
               when 'Y' | 'y' => goto State_128;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_41>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'R' | 'T' .. 'Z' | 'a' .. 'r' | 't' .. 'z' => goto State_78;
               when 'S' | 's' => goto State_129;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_42>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'G' | 'I' .. 'H' | 'J' .. 'Z' | 'a' .. 'g' | 'i' .. 'h' | 'j' .. 'z' => goto State_78;
               when 'H' | 'h' => goto State_130;
               when 'I' | 'i' => goto State_131;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_43>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'N' | 'P' .. 'Z' | 'a' .. 'n' | 'p' .. 'z' => goto State_78;
               when 'O' | 'o' => goto State_132;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_44>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Brack_Open;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_133;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_45>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Brack_Close;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_46>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Pipe;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_47>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_48>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_49>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_50>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Whitespace;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#9#) .. Character_Type'Val (16#d#) | ' ' => goto State_50;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_51>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. 'Z' | '\' .. Character_Type'Val (16#10ffff#) => goto State_51;
               when '"' => goto State_52;
               when '[' => goto State_53;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_52>>

               Match_Index := Index - 1;
               Match_Kind := Ada_String;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_134;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_53>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. 'Z' | '\' .. Character_Type'Val (16#10ffff#) => goto State_51;
               when '"' => goto State_135;
               when '[' => goto State_53;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_54>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Prep_Line;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#10ffff#) => goto State_136;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_55>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_133;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_56>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '$' | '&' .. 'Z' | '\' .. Character_Type'Val (16#10ffff#) => goto State_56;
               when '%' => goto State_57;
               when '[' => goto State_58;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_57>>

               Match_Index := Index - 1;
               Match_Kind := Ada_String;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '%' => goto State_137;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_58>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. '$' | '&' .. 'Z' | '\' .. Character_Type'Val (16#10ffff#) => goto State_56;
               when '"' => goto State_138;
               when '%' => goto State_57;
               when '[' => goto State_58;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_59>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when ''' => goto State_139;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_60>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_140;
               when ''' => goto State_139;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_61>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Power;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_62>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Comment;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#10ffff#) => goto State_141;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_63>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Doubledot;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_64>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Notequal;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_65>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' => goto State_142;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_66>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_143;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_67>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Integer;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '#' | ':' => goto State_65;
               when '.' => goto State_66;
               when '0' .. '9' => goto State_144;
               when 'E' | 'e' => goto State_68;
               when '_' => goto State_145;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_68>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '+' => goto State_146;
               when '-' => goto State_147;
               when '0' .. '9' => goto State_148;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_69>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_149;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_70>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Assign;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_71>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Label_Start;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_72>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Lte;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_73>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Diamond;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_74>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Arrow;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_75>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Gte;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_76>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Label_End;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_77>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_78>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_79>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'N' | 'P' .. 'R' | 'T' .. 'Z' | 'a' .. 'n' | 'p' .. 'r' | 't' .. 'z' => goto State_78;
               when 'O' | 'o' => goto State_150;
               when 'S' | 's' => goto State_151;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_80>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'B' | 'D' .. 'Z' | 'a' .. 'b' | 'd' .. 'z' => goto State_78;
               when 'C' | 'c' => goto State_152;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_81>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'K' | 'M' .. 'Z' | 'a' .. 'k' | 'm' .. 'z' => goto State_78;
               when 'L' | 'l' => goto State_153;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_82>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'C' | 'E' .. 'Z' | 'a' .. 'c' | 'e' .. 'z' => goto State_78;
               when 'D' | 'd' => goto State_154;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_83>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Q' | 'S' .. 'Z' | 'a' .. 'q' | 's' .. 'z' => goto State_78;
               when 'R' | 'r' => goto State_155;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_84>>

               Match_Index := Index - 1;
               Match_Kind := Ada_At;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_85>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_156;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_86>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_87>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_88>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_89>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_90>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_91>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_92>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'F' | 'H' .. 'Z' | 'a' .. 'f' | 'h' .. 'z' => goto State_78;
               when 'G' | 'g' => goto State_157;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_93>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'C' | 'E' .. 'Z' | 'a' .. 'c' | 'e' .. 'z' => goto State_78;
               when 'D' | 'd' => goto State_158;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_94>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'R' | 'T' .. 'Z' | 'a' .. 'r' | 't' .. 'z' => goto State_78;
               when 'S' | 's' => goto State_159;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_95>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'M' | 'O' .. 'Z' | 'a' .. 'm' | 'o' .. 'z' => goto State_78;
               when 'N' | 'n' => goto State_160;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_96>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'B' | 'D' .. 'K' | 'M' .. 'Z' | 'a' .. 'b' | 'd' .. 'k' | 'm' .. 'z' => goto State_78;
               when 'C' | 'c' => goto State_161;
               when 'L' | 'l' => goto State_162;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_97>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'F' | 'H' .. 'Z' | 'a' .. 'f' | 'h' .. 'z' => goto State_78;
               when 'G' | 'g' => goto State_163;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_98>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Do;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_99>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'R' | 'T' .. 'Z' | 'a' .. 'r' | 't' .. 'z' => goto State_78;
               when 'S' | 's' => goto State_164;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_100>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'C' | 'E' .. 'S' | 'U' .. 'Z' | 'a' .. 'c' | 'e' .. 's' | 'u' .. 'z' => goto State_78;
               when 'D' | 'd' => goto State_165;
               when 'T' | 't' => goto State_166;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_101>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'B' | 'D' .. 'H' | 'J' .. 'Z' | 'a' .. 'b' | 'd' .. 'h' | 'j' .. 'z' => goto State_78;
               when 'C' | 'c' => goto State_167;
               when 'I' | 'i' => goto State_168;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_102>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Q' | 'S' .. 'Z' | 'a' .. 'q' | 's' .. 'z' => goto State_78;
               when 'R' | 'r' => goto State_169;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_103>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'M' | 'O' .. 'Z' | 'a' .. 'm' | 'o' .. 'z' => goto State_78;
               when 'N' | 'n' => goto State_170;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_104>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'M' | 'O' .. 'Z' | 'a' .. 'm' | 'o' .. 'z' => goto State_78;
               when 'N' | 'n' => goto State_171;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_105>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_172;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_106>>

               Match_Index := Index - 1;
               Match_Kind := Ada_If;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_107>>

               Match_Index := Index - 1;
               Match_Kind := Ada_In;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_108>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Is;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_109>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'L' | 'N' .. 'Z' | 'a' .. 'l' | 'n' .. 'z' => goto State_78;
               when 'M' | 'm' => goto State_173;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_110>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'N' | 'P' .. 'Z' | 'a' .. 'n' | 'p' .. 'z' => goto State_78;
               when 'O' | 'o' => goto State_174;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_111>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'C' | 'E' .. 'Z' | 'a' .. 'c' | 'e' .. 'z' => goto State_78;
               when 'D' | 'd' => goto State_175;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_112>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'V' | 'X' .. 'Z' | 'a' .. 'v' | 'x' .. 'z' => goto State_78;
               when 'W' | 'w' => goto State_176;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_113>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_177;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_114>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'K' | 'M' .. 'Z' | 'a' .. 'k' | 'm' .. 'z' => goto State_78;
               when 'L' | 'l' => goto State_178;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_115>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Of;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_116>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Or;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_117>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'G' | 'I' .. 'Z' | 'a' .. 'g' | 'i' .. 'z' => goto State_78;
               when 'H' | 'h' => goto State_179;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_118>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_180;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_119>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'B' | 'D' .. 'Z' | 'a' .. 'b' | 'd' .. 'z' => goto State_78;
               when 'C' | 'c' => goto State_181;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_120>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_182;
               when 'B' .. 'H' | 'J' .. 'N' | 'P' .. 'Z' | 'b' .. 'h' | 'j' .. 'n' | 'p' .. 'z' => goto State_78;
               when 'I' | 'i' => goto State_183;
               when 'O' | 'o' => goto State_184;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_121>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'H' | 'J' .. 'M' | 'O' .. 'Z' | 'a' .. 'h' | 'j' .. 'm' | 'o' .. 'z' => goto State_78;
               when 'I' | 'i' => goto State_185;
               when 'N' | 'n' => goto State_186;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_122>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'B' | 'D' .. 'L' | 'N' .. 'M' | 'O' .. 'S' | 'U' | 'W' .. 'Z' | 'a' .. 'b' | 'd' .. 'l' | 'n' .. 'm' | 'o' .. 's' | 'u' | 'w' .. 'z' => goto State_78;
               when 'C' | 'c' => goto State_187;
               when 'M' | 'm' => goto State_188;
               when 'N' | 'n' => goto State_189;
               when 'T' | 't' => goto State_190;
               when 'V' | 'v' => goto State_191;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_123>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'K' | 'M' .. 'O' | 'Q' .. 'Z' | 'a' .. 'k' | 'm' .. 'o' | 'q' .. 'z' => goto State_78;
               when 'L' | 'l' => goto State_192;
               when 'P' | 'p' => goto State_193;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_124>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'C' .. 'Z' | 'a' | 'c' .. 'z' => goto State_78;
               when 'B' | 'b' => goto State_194;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_125>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'R' | 'T' .. 'Z' | 'a' .. 'r' | 't' .. 'z' => goto State_78;
               when 'S' | 's' => goto State_195;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_126>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Q' | 'S' .. 'Z' | 'a' .. 'q' | 's' .. 'z' => goto State_78;
               when 'R' | 'r' => goto State_196;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_127>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_197;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_128>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'O' | 'Q' .. 'Z' | 'a' .. 'o' | 'q' .. 'z' => goto State_78;
               when 'P' | 'p' => goto State_198;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_129>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_199;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_130>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'H' | 'J' .. 'Z' | 'a' .. 'd' | 'f' .. 'h' | 'j' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_200;
               when 'I' | 'i' => goto State_201;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_131>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_202;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_132>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Q' | 'S' .. 'Z' | 'a' .. 'q' | 's' .. 'z' => goto State_78;
               when 'R' | 'r' => goto State_203;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_133>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => goto State_204;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_134>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. 'Z' | '\' .. Character_Type'Val (16#10ffff#) => goto State_51;
               when '"' => goto State_52;
               when '[' => goto State_53;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_135>>

               Match_Index := Index - 1;
               Match_Kind := Ada_String;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_134;
               when '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => goto State_205;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_136>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Prep_Line;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#10ffff#) => goto State_136;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_137>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '$' | '&' .. 'Z' | '\' .. Character_Type'Val (16#10ffff#) => goto State_56;
               when '%' => goto State_57;
               when '[' => goto State_58;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_138>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '$' | '&' .. '/' | ':' .. '@' | 'G' .. 'Z' | '\' .. '`' | 'g' .. Character_Type'Val (16#10ffff#) => goto State_56;
               when '%' => goto State_57;
               when '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => goto State_206;
               when '[' => goto State_58;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_139>>

               case Self.Last_Token_Kind is
                     when Ada_Identifier =>
                        Match_Kind := Ada_Tick;
                        Match_Index := Index - 1 - 2;
                     when others =>
                        Match_Kind := Ada_Char;
                        Match_Index := Index - 1 - 0;
               end case;


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_140>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => goto State_207;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_141>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Comment;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#10ffff#) => goto State_208;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_142>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '#' | ':' => goto State_209;
               when '.' => goto State_210;
               when '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' => goto State_211;
               when '_' => goto State_212;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_143>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Decimal;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_213;
               when 'E' | 'e' => goto State_214;
               when '_' => goto State_215;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_144>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Integer;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '#' | ':' => goto State_65;
               when '.' => goto State_66;
               when '0' .. '9' => goto State_144;
               when 'E' | 'e' => goto State_68;
               when '_' => goto State_145;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_145>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_216;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_146>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_148;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_147>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_148;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_148>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Integer;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_217;
               when '_' => goto State_218;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_149>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Integer;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '#' | ':' => goto State_65;
               when '.' => goto State_66;
               when '0' .. '9' => goto State_67;
               when 'E' | 'e' => goto State_68;
               when '_' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_150>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Q' | 'S' .. 'Z' | 'a' .. 'q' | 's' .. 'z' => goto State_78;
               when 'R' | 'r' => goto State_219;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_151>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Abs;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_152>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_220;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_153>>

               Match_Index := Index - 1;
               Match_Kind := Ada_All;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_154>>

               Match_Index := Index - 1;
               Match_Kind := Ada_And;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_155>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_221;
               when 'B' .. 'Z' | 'b' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_156>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => goto State_222;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_157>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'H' | 'J' .. 'Z' | 'a' .. 'h' | 'j' .. 'z' => goto State_78;
               when 'I' | 'i' => goto State_223;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_158>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'X' | 'Z' | 'a' .. 'x' | 'z' => goto State_78;
               when 'Y' | 'y' => goto State_224;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_159>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_225;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_160>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'R' | 'T' .. 'Z' | 'a' .. 'r' | 't' .. 'z' => goto State_78;
               when 'S' | 's' => goto State_226;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_161>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'K' | 'M' .. 'Z' | 'a' .. 'k' | 'm' .. 'z' => goto State_78;
               when 'L' | 'l' => goto State_227;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_162>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_228;
               when 'B' .. 'S' | 'U' .. 'Z' | 'b' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_229;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_163>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'H' | 'J' .. 'Z' | 'a' .. 'h' | 'j' .. 'z' => goto State_78;
               when 'I' | 'i' => goto State_230;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_164>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'H' | 'J' .. 'Z' | 'a' .. 'd' | 'f' .. 'h' | 'j' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_231;
               when 'I' | 'i' => goto State_232;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_165>>

               Match_Index := Index - 1;
               Match_Kind := Ada_End;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_166>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Q' | 'S' .. 'Z' | 'a' .. 'q' | 's' .. 'z' => goto State_78;
               when 'R' | 'r' => goto State_233;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_167>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_234;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_168>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_235;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_169>>

               Match_Index := Index - 1;
               Match_Kind := Ada_For;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_170>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'B' | 'D' .. 'Z' | 'a' .. 'b' | 'd' .. 'z' => goto State_78;
               when 'C' | 'c' => goto State_236;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_171>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_237;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_172>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'N' | 'P' .. 'Z' | 'a' .. 'n' | 'p' .. 'z' => goto State_78;
               when 'O' | 'o' => goto State_238;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_173>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'H' | 'J' .. 'Z' | 'a' .. 'h' | 'j' .. 'z' => goto State_78;
               when 'I' | 'i' => goto State_239;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_174>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'O' | 'Q' .. 'Z' | 'a' .. 'o' | 'q' .. 'z' => goto State_78;
               when 'P' | 'p' => goto State_240;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_175>>

               case Self.Last_Token_Kind is
                     when Ada_Tick =>
                        Match_Kind := Ada_Identifier;
                        Match_Index := Index - 1 - 0;
                     when others =>
                        Match_Kind := Ada_Mod;
                        Match_Index := Index - 1 - 0;
               end case;


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_176>>

               Match_Index := Index - 1;
               Match_Kind := Ada_New;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_177>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Not;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_178>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'K' | 'M' .. 'Z' | 'a' .. 'k' | 'm' .. 'z' => goto State_78;
               when 'L' | 'l' => goto State_241;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_179>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_242;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_180>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Out;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_181>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'J' | 'L' .. 'Z' | 'a' .. 'j' | 'l' .. 'z' => goto State_78;
               when 'K' | 'k' => goto State_243;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_182>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'F' | 'H' .. 'Z' | 'a' .. 'f' | 'h' .. 'z' => goto State_78;
               when 'G' | 'g' => goto State_244;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_183>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'U' | 'W' .. 'Z' | 'a' .. 'u' | 'w' .. 'z' => goto State_78;
               when 'V' | 'v' => goto State_245;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_184>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'B' | 'D' .. 'Z' | 'a' .. 'b' | 'd' .. 'z' => goto State_78;
               when 'C' | 'c' => goto State_246;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_185>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'R' | 'T' .. 'Z' | 'a' .. 'r' | 't' .. 'z' => goto State_78;
               when 'S' | 's' => goto State_247;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_186>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'F' | 'H' .. 'Z' | 'a' .. 'f' | 'h' .. 'z' => goto State_78;
               when 'G' | 'g' => goto State_248;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_187>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'N' | 'P' .. 'Z' | 'a' .. 'n' | 'p' .. 'z' => goto State_78;
               when 'O' | 'o' => goto State_249;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_188>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Rem;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_189>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_250;
               when 'B' .. 'Z' | 'b' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_190>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'T' | 'V' .. 'Z' | 'a' .. 't' | 'v' .. 'z' => goto State_78;
               when 'U' | 'u' => goto State_251;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_191>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_252;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_192>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_253;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_193>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_254;
               when 'B' .. 'Z' | 'b' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_194>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_255;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_195>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'J' | 'L' .. 'Z' | 'a' .. 'j' | 'l' .. 'z' => goto State_78;
               when 'K' | 'k' => goto State_256;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_196>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'L' | 'N' .. 'Z' | 'a' .. 'l' | 'n' .. 'z' => goto State_78;
               when 'M' | 'm' => goto State_257;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_197>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'M' | 'O' .. 'Z' | 'a' .. 'm' | 'o' .. 'z' => goto State_78;
               when 'N' | 'n' => goto State_258;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_198>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_259;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_199>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Use;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_200>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'M' | 'O' .. 'Z' | 'a' .. 'm' | 'o' .. 'z' => goto State_78;
               when 'N' | 'n' => goto State_260;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_201>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'K' | 'M' .. 'Z' | 'a' .. 'k' | 'm' .. 'z' => goto State_78;
               when 'L' | 'l' => goto State_261;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_202>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'G' | 'I' .. 'Z' | 'a' .. 'g' | 'i' .. 'z' => goto State_78;
               when 'H' | 'h' => goto State_262;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_203>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Xor;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_204>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_263;
               when '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => goto State_264;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_205>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_265;
               when '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => goto State_266;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_206>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. '$' | '&' .. '/' | ':' .. '@' | 'G' .. 'Z' | '\' .. '`' | 'g' .. Character_Type'Val (16#10ffff#) => goto State_56;
               when '"' => goto State_267;
               when '%' => goto State_57;
               when '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => goto State_268;
               when '[' => goto State_58;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_207>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_269;
               when '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => goto State_270;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_208>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Comment;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#10ffff#) => goto State_208;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_209>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Integer;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when 'E' | 'e' => goto State_271;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_210>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' => goto State_272;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_211>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '#' | ':' => goto State_209;
               when '.' => goto State_210;
               when '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' => goto State_211;
               when '_' => goto State_212;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_212>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' => goto State_211;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_213>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Decimal;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_213;
               when 'E' | 'e' => goto State_214;
               when '_' => goto State_215;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_214>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '+' => goto State_273;
               when '-' => goto State_274;
               when '0' .. '9' => goto State_275;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_215>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_213;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_216>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Integer;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '#' | ':' => goto State_65;
               when '.' => goto State_66;
               when '0' .. '9' => goto State_144;
               when 'E' | 'e' => goto State_68;
               when '_' => goto State_145;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_217>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Integer;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_217;
               when '_' => goto State_218;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_218>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_217;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_219>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_276;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_220>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'O' | 'Q' .. 'R' | 'T' .. 'Z' | 'a' .. 'o' | 'q' .. 'r' | 't' .. 'z' => goto State_78;
               when 'P' | 'p' => goto State_277;
               when 'S' | 's' => goto State_278;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_221>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'X' | 'Z' | 'a' .. 'x' | 'z' => goto State_78;
               when 'Y' | 'y' => goto State_279;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_222>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_280;
               when '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => goto State_281;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_223>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'M' | 'O' .. 'Z' | 'a' .. 'm' | 'o' .. 'z' => goto State_78;
               when 'N' | 'n' => goto State_282;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_224>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Body;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_225>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Case;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_226>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_283;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_227>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_284;
               when 'B' .. 'Z' | 'b' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_228>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'X' | 'Z' | 'a' .. 'x' | 'z' => goto State_78;
               when 'Y' | 'y' => goto State_285;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_229>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_286;
               when 'B' .. 'Z' | 'b' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_230>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_287;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_231>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Else;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_232>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'E' | 'G' .. 'Z' | 'a' .. 'e' | 'g' .. 'z' => goto State_78;
               when 'F' | 'f' => goto State_288;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_233>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'X' | 'Z' | 'a' .. 'x' | 'z' => goto State_78;
               when 'Y' | 'y' => goto State_289;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_234>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'O' | 'Q' .. 'Z' | 'a' .. 'o' | 'q' .. 'z' => goto State_78;
               when 'P' | 'p' => goto State_290;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_235>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Exit;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_236>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_291;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_237>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Q' | 'S' .. 'Z' | 'a' .. 'q' | 's' .. 'z' => goto State_78;
               when 'R' | 'r' => goto State_292;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_238>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Goto;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_239>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_293;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_240>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Loop;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_241>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Null;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_242>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Q' | 'S' .. 'Z' | 'a' .. 'q' | 's' .. 'z' => goto State_78;
               when 'R' | 'r' => goto State_294;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_243>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_295;
               when 'B' .. 'Z' | 'b' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_244>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'L' | 'N' .. 'Z' | 'a' .. 'l' | 'n' .. 'z' => goto State_78;
               when 'M' | 'm' => goto State_296;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_245>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_297;
               when 'B' .. 'Z' | 'b' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_246>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_298;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_247>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_299;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_248>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_300;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_249>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Q' | 'S' .. 'Z' | 'a' .. 'q' | 's' .. 'z' => goto State_78;
               when 'R' | 'r' => goto State_301;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_250>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'L' | 'N' .. 'Z' | 'a' .. 'l' | 'n' .. 'z' => goto State_78;
               when 'M' | 'm' => goto State_302;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_251>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Q' | 'S' .. 'Z' | 'a' .. 'q' | 's' .. 'z' => goto State_78;
               when 'R' | 'r' => goto State_303;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_252>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Q' | 'S' .. 'Z' | 'a' .. 'q' | 's' .. 'z' => goto State_78;
               when 'R' | 'r' => goto State_304;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_253>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'B' | 'D' .. 'Z' | 'a' .. 'b' | 'd' .. 'z' => goto State_78;
               when 'C' | 'c' => goto State_305;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_254>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Q' | 'S' .. 'Z' | 'a' .. 'q' | 's' .. 'z' => goto State_78;
               when 'R' | 'r' => goto State_306;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_255>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'X' | 'Z' | 'a' .. 'x' | 'z' => goto State_78;
               when 'Y' | 'y' => goto State_307;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_256>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Task;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_257>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'H' | 'J' .. 'Z' | 'a' .. 'h' | 'j' .. 'z' => goto State_78;
               when 'I' | 'i' => goto State_308;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_258>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Then;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_259>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Type;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_260>>

               Match_Index := Index - 1;
               Match_Kind := Ada_When;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_261>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_309;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_262>>

               Match_Index := Index - 1;
               Match_Kind := Ada_With;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_263>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when ']' => goto State_310;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_264>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_263;
               when '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => goto State_264;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_265>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when ']' => goto State_311;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_266>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_265;
               when '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => goto State_266;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_267>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '$' | '&' .. 'Z' | '\' | '^' .. Character_Type'Val (16#10ffff#) => goto State_56;
               when '%' => goto State_57;
               when '[' => goto State_58;
               when ']' => goto State_312;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_268>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. '$' | '&' .. '/' | ':' .. '@' | 'G' .. 'Z' | '\' .. '`' | 'g' .. Character_Type'Val (16#10ffff#) => goto State_56;
               when '"' => goto State_267;
               when '%' => goto State_57;
               when '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => goto State_268;
               when '[' => goto State_58;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_269>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when ']' => goto State_313;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_270>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_269;
               when '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => goto State_270;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_271>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '+' => goto State_314;
               when '-' => goto State_315;
               when '0' .. '9' => goto State_316;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_272>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '#' | ':' => goto State_317;
               when '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' => goto State_318;
               when '_' => goto State_319;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_273>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_275;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_274>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_275;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_275>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Decimal;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_320;
               when '_' => goto State_321;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_276>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Abort;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_277>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_322;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_278>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'R' | 'T' .. 'Z' | 'a' .. 'r' | 't' .. 'z' => goto State_78;
               when 'S' | 's' => goto State_323;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_279>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Array;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_280>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when ']' => goto State_324;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_281>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_280;
               when '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => goto State_281;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_282>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Begin;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_283>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_325;
               when 'B' .. 'Z' | 'b' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_284>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Q' | 'S' .. 'Z' | 'a' .. 'q' | 's' .. 'z' => goto State_78;
               when 'R' | 'r' => goto State_326;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_285>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Delay;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_286>>

               case Self.Last_Token_Kind is
                     when Ada_Tick =>
                        Match_Kind := Ada_Identifier;
                        Match_Index := Index - 1 - 0;
                     when others =>
                        Match_Kind := Ada_Delta;
                        Match_Index := Index - 1 - 0;
               end case;


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_287>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'R' | 'T' .. 'Z' | 'a' .. 'r' | 't' .. 'z' => goto State_78;
               when 'S' | 's' => goto State_327;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_288>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Elsif;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_289>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Entry;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_290>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_328;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_291>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'H' | 'J' .. 'Z' | 'a' .. 'h' | 'j' .. 'z' => goto State_78;
               when 'I' | 'i' => goto State_329;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_292>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'H' | 'J' .. 'Z' | 'a' .. 'h' | 'j' .. 'z' => goto State_78;
               when 'I' | 'i' => goto State_330;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_293>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_331;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_294>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'R' | 'T' .. 'Z' | 'a' .. 'r' | 't' .. 'z' => goto State_78;
               when 'S' | 's' => goto State_332;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_295>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'F' | 'H' .. 'Z' | 'a' .. 'f' | 'h' .. 'z' => goto State_78;
               when 'G' | 'g' => goto State_333;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_296>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_334;
               when 'B' .. 'Z' | 'b' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_297>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_335;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_298>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'C' | 'E' .. 'Z' | 'a' .. 'c' | 'e' .. 'z' => goto State_78;
               when 'D' | 'd' => goto State_336;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_299>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Raise;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_300>>

               case Self.Last_Token_Kind is
                     when Ada_Tick =>
                        Match_Kind := Ada_Identifier;
                        Match_Index := Index - 1 - 0;
                     when others =>
                        Match_Kind := Ada_Range;
                        Match_Index := Index - 1 - 0;
               end case;


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_301>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'C' | 'E' .. 'Z' | 'a' .. 'c' | 'e' .. 'z' => goto State_78;
               when 'D' | 'd' => goto State_337;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_302>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_338;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_303>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'M' | 'O' .. 'Z' | 'a' .. 'm' | 'o' .. 'z' => goto State_78;
               when 'N' | 'n' => goto State_339;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_304>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'R' | 'T' .. 'Z' | 'a' .. 'r' | 't' .. 'z' => goto State_78;
               when 'S' | 's' => goto State_340;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_305>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_341;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_306>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_342;
               when 'B' .. 'Z' | 'b' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_307>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'O' | 'Q' .. 'Z' | 'a' .. 'o' | 'q' .. 'z' => goto State_78;
               when 'P' | 'p' => goto State_343;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_308>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'M' | 'O' .. 'Z' | 'a' .. 'm' | 'o' .. 'z' => goto State_78;
               when 'N' | 'n' => goto State_344;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_309>>

               Match_Index := Index - 1;
               Match_Kind := Ada_While;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_310>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_311>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. 'Z' | '\' .. Character_Type'Val (16#10ffff#) => goto State_51;
               when '"' => goto State_52;
               when '[' => goto State_53;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_312>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '$' | '&' .. 'Z' | '\' .. Character_Type'Val (16#10ffff#) => goto State_56;
               when '%' => goto State_57;
               when '[' => goto State_58;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_313>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when ''' => goto State_345;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_314>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_316;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_315>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_316;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_316>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Integer;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_346;
               when '_' => goto State_347;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_317>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Decimal;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when 'E' | 'e' => goto State_348;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_318>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '#' | ':' => goto State_317;
               when '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' => goto State_318;
               when '_' => goto State_319;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_319>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' => goto State_318;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_320>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Decimal;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_320;
               when '_' => goto State_321;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_321>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_320;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_322>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Accept;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_323>>

               case Self.Last_Token_Kind is
                     when Ada_Tick =>
                        Match_Kind := Ada_Identifier;
                        Match_Index := Index - 1 - 0;
                     when others =>
                        Match_Kind := Ada_Access;
                        Match_Index := Index - 1 - 0;
               end case;


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_324>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_325>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'M' | 'O' .. 'Z' | 'a' .. 'm' | 'o' .. 'z' => goto State_78;
               when 'N' | 'n' => goto State_349;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_326>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_350;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_327>>

               case Self.Last_Token_Kind is
                     when Ada_Tick =>
                        Match_Kind := Ada_Identifier;
                        Match_Index := Index - 1 - 0;
                     when others =>
                        Match_Kind := Ada_Digits;
                        Match_Index := Index - 1 - 0;
               end case;


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_328>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'H' | 'J' .. 'Z' | 'a' .. 'h' | 'j' .. 'z' => goto State_78;
               when 'I' | 'i' => goto State_351;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_329>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'N' | 'P' .. 'Z' | 'a' .. 'n' | 'p' .. 'z' => goto State_78;
               when 'O' | 'o' => goto State_352;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_330>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'B' | 'D' .. 'Z' | 'a' .. 'b' | 'd' .. 'z' => goto State_78;
               when 'C' | 'c' => goto State_353;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_331>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'C' | 'E' .. 'Z' | 'a' .. 'c' | 'e' .. 'z' => goto State_78;
               when 'D' | 'd' => goto State_354;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_332>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Others;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_333>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_355;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_334>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Pragma;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_335>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_356;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_336>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'T' | 'V' .. 'Z' | 'a' .. 't' | 'v' .. 'z' => goto State_78;
               when 'U' | 'u' => goto State_357;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_337>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Record;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_338>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'R' | 'T' .. 'Z' | 'a' .. 'r' | 't' .. 'z' => goto State_78;
               when 'S' | 's' => goto State_358;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_339>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Return;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_340>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_359;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_341>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Select;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_342>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_360;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_343>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_361;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_344>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' | 'a' => goto State_362;
               when 'B' .. 'Z' | 'b' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_345>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Char;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_346>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Integer;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_346;
               when '_' => goto State_347;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_347>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_346;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_348>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '+' => goto State_363;
               when '-' => goto State_364;
               when '0' .. '9' => goto State_365;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_349>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_366;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_350>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Declare;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_351>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'N' | 'P' .. 'Z' | 'a' .. 'n' | 'p' .. 'z' => goto State_78;
               when 'O' | 'o' => goto State_367;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_352>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'M' | 'O' .. 'Z' | 'a' .. 'm' | 'o' .. 'z' => goto State_78;
               when 'N' | 'n' => goto State_368;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_353>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Generic;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_354>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Limited;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_355>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Package;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_356>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Private;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_357>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Q' | 'S' .. 'Z' | 'a' .. 'q' | 's' .. 'z' => goto State_78;
               when 'R' | 'r' => goto State_369;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_358>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Renames;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_359>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Reverse;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_360>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_370;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_361>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Subtype;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_362>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'S' | 'U' .. 'Z' | 'a' .. 's' | 'u' .. 'z' => goto State_78;
               when 'T' | 't' => goto State_371;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_363>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_365;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_364>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_365;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_365>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Decimal;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_372;
               when '_' => goto State_373;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_366>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Constant;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_367>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'M' | 'O' .. 'Z' | 'a' .. 'm' | 'o' .. 'z' => goto State_78;
               when 'N' | 'n' => goto State_374;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_368>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Function;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_369>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_375;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_370>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Separate;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_371>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'D' | 'F' .. 'Z' | 'a' .. 'd' | 'f' .. 'z' => goto State_78;
               when 'E' | 'e' => goto State_376;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_372>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Decimal;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_372;
               when '_' => goto State_373;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_373>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_372;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_374>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Exception;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_375>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Procedure;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;

            <<State_376>>

               Match_Index := Index - 1;
               Match_Kind := Ada_Terminate;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_77;
               when 'A' .. 'Z' | 'a' .. 'z' => goto State_78;
               when '[' => goto State_85;
               when '_' => goto State_86;

            when others =>
               if Input_Char > Character_Type'Val (127) then
                     if Contains (Input_Char, Ranges_4) then
                        goto State_77;
                     end if;
                     if Contains (Input_Char, Ranges_0) then
                        goto State_78;
                     end if;
                     if Contains (Input_Char, Ranges_1) then
                        goto State_87;
                     end if;
                     if Contains (Input_Char, Ranges_2) then
                        goto State_88;
                     end if;
                     if Contains (Input_Char, Ranges_5) then
                        goto State_89;
                     end if;
                     if Contains (Input_Char, Ranges_6) then
                        goto State_90;
                     end if;
                     if Contains (Input_Char, Ranges_3) then
                        goto State_91;
                     end if;
               end if;

               goto Stop;
            end case;
         end;


      <<Stop>>
      --  We end up here as soon as the currently analyzed character was not
      --  accepted by any transitions from the current state. Two cases from
      --  there:

      if Match_Index = 0 then
         --  We haven't found a match. Just create an error token and plan to
         --  start a new token at the next character.
         if Index > Self.Input_Last then
            Token := (Ada_Termination, Index, Index - 1);
            Self.Has_Next := False;
         else
            Token := (Ada_Lexing_Failure, First_Index, First_Index);
         end if;

      elsif Match_Ignore then
         --  We found a match. It must be ignored: resume lexing to start right
         --  after the matched text.
         First_Index := Match_Index + 1;
         goto Start;

      else
         --  We found a match for which we must emit a token
         Token := (Match_Kind, First_Index, Match_Index);
      end if;

      Self.Last_Token := Token;
      if not Is_Trivia (Token.Kind) then
         Self.Last_Token_Kind := Token.Kind;
      end if;
   end Next_Token;

end Libadalang.Lexer_State_Machine;
