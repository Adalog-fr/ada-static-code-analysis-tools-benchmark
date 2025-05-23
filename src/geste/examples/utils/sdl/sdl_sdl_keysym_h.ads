pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package SDL_SDL_keysym_h is

   --  unsupported macro: KMOD_CTRL (KMOD_LCTRL|KMOD_RCTRL)
   --  unsupported macro: KMOD_SHIFT (KMOD_LSHIFT|KMOD_RSHIFT)
   --  unsupported macro: KMOD_ALT (KMOD_LALT|KMOD_RALT)
   --  unsupported macro: KMOD_META (KMOD_LMETA|KMOD_RMETA)
   subtype SDLKey is unsigned;
   SDLK_UNKNOWN : constant SDLKey := 0;
   SDLK_FIRST : constant SDLKey := 0;
   SDLK_BACKSPACE : constant SDLKey := 8;
   SDLK_TAB : constant SDLKey := 9;
   SDLK_CLEAR : constant SDLKey := 12;
   SDLK_RETURN : constant SDLKey := 13;
   SDLK_PAUSE : constant SDLKey := 19;
   SDLK_ESCAPE : constant SDLKey := 27;
   SDLK_SPACE : constant SDLKey := 32;
   SDLK_EXCLAIM : constant SDLKey := 33;
   SDLK_QUOTEDBL : constant SDLKey := 34;
   SDLK_HASH : constant SDLKey := 35;
   SDLK_DOLLAR : constant SDLKey := 36;
   SDLK_AMPERSAND : constant SDLKey := 38;
   SDLK_QUOTE : constant SDLKey := 39;
   SDLK_LEFTPAREN : constant SDLKey := 40;
   SDLK_RIGHTPAREN : constant SDLKey := 41;
   SDLK_ASTERISK : constant SDLKey := 42;
   SDLK_PLUS : constant SDLKey := 43;
   SDLK_COMMA : constant SDLKey := 44;
   SDLK_MINUS : constant SDLKey := 45;
   SDLK_PERIOD : constant SDLKey := 46;
   SDLK_SLASH : constant SDLKey := 47;
   SDLK_0 : constant SDLKey := 48;
   SDLK_1 : constant SDLKey := 49;
   SDLK_2 : constant SDLKey := 50;
   SDLK_3 : constant SDLKey := 51;
   SDLK_4 : constant SDLKey := 52;
   SDLK_5 : constant SDLKey := 53;
   SDLK_6 : constant SDLKey := 54;
   SDLK_7 : constant SDLKey := 55;
   SDLK_8 : constant SDLKey := 56;
   SDLK_9 : constant SDLKey := 57;
   SDLK_COLON : constant SDLKey := 58;
   SDLK_SEMICOLON : constant SDLKey := 59;
   SDLK_LESS : constant SDLKey := 60;
   SDLK_EQUALS : constant SDLKey := 61;
   SDLK_GREATER : constant SDLKey := 62;
   SDLK_QUESTION : constant SDLKey := 63;
   SDLK_AT : constant SDLKey := 64;
   SDLK_LEFTBRACKET : constant SDLKey := 91;
   SDLK_BACKSLASH : constant SDLKey := 92;
   SDLK_RIGHTBRACKET : constant SDLKey := 93;
   SDLK_CARET : constant SDLKey := 94;
   SDLK_UNDERSCORE : constant SDLKey := 95;
   SDLK_BACKQUOTE : constant SDLKey := 96;
   SDLK_a : constant SDLKey := 97;
   SDLK_b : constant SDLKey := 98;
   SDLK_c : constant SDLKey := 99;
   SDLK_d : constant SDLKey := 100;
   SDLK_e : constant SDLKey := 101;
   SDLK_f : constant SDLKey := 102;
   SDLK_g : constant SDLKey := 103;
   SDLK_h : constant SDLKey := 104;
   SDLK_i : constant SDLKey := 105;
   SDLK_j : constant SDLKey := 106;
   SDLK_k : constant SDLKey := 107;
   SDLK_l : constant SDLKey := 108;
   SDLK_m : constant SDLKey := 109;
   SDLK_n : constant SDLKey := 110;
   SDLK_o : constant SDLKey := 111;
   SDLK_p : constant SDLKey := 112;
   SDLK_q : constant SDLKey := 113;
   SDLK_r : constant SDLKey := 114;
   SDLK_s : constant SDLKey := 115;
   SDLK_t : constant SDLKey := 116;
   SDLK_u : constant SDLKey := 117;
   SDLK_v : constant SDLKey := 118;
   SDLK_w : constant SDLKey := 119;
   SDLK_x : constant SDLKey := 120;
   SDLK_y : constant SDLKey := 121;
   SDLK_z : constant SDLKey := 122;
   SDLK_DELETE : constant SDLKey := 127;
   SDLK_WORLD_0 : constant SDLKey := 160;
   SDLK_WORLD_1 : constant SDLKey := 161;
   SDLK_WORLD_2 : constant SDLKey := 162;
   SDLK_WORLD_3 : constant SDLKey := 163;
   SDLK_WORLD_4 : constant SDLKey := 164;
   SDLK_WORLD_5 : constant SDLKey := 165;
   SDLK_WORLD_6 : constant SDLKey := 166;
   SDLK_WORLD_7 : constant SDLKey := 167;
   SDLK_WORLD_8 : constant SDLKey := 168;
   SDLK_WORLD_9 : constant SDLKey := 169;
   SDLK_WORLD_10 : constant SDLKey := 170;
   SDLK_WORLD_11 : constant SDLKey := 171;
   SDLK_WORLD_12 : constant SDLKey := 172;
   SDLK_WORLD_13 : constant SDLKey := 173;
   SDLK_WORLD_14 : constant SDLKey := 174;
   SDLK_WORLD_15 : constant SDLKey := 175;
   SDLK_WORLD_16 : constant SDLKey := 176;
   SDLK_WORLD_17 : constant SDLKey := 177;
   SDLK_WORLD_18 : constant SDLKey := 178;
   SDLK_WORLD_19 : constant SDLKey := 179;
   SDLK_WORLD_20 : constant SDLKey := 180;
   SDLK_WORLD_21 : constant SDLKey := 181;
   SDLK_WORLD_22 : constant SDLKey := 182;
   SDLK_WORLD_23 : constant SDLKey := 183;
   SDLK_WORLD_24 : constant SDLKey := 184;
   SDLK_WORLD_25 : constant SDLKey := 185;
   SDLK_WORLD_26 : constant SDLKey := 186;
   SDLK_WORLD_27 : constant SDLKey := 187;
   SDLK_WORLD_28 : constant SDLKey := 188;
   SDLK_WORLD_29 : constant SDLKey := 189;
   SDLK_WORLD_30 : constant SDLKey := 190;
   SDLK_WORLD_31 : constant SDLKey := 191;
   SDLK_WORLD_32 : constant SDLKey := 192;
   SDLK_WORLD_33 : constant SDLKey := 193;
   SDLK_WORLD_34 : constant SDLKey := 194;
   SDLK_WORLD_35 : constant SDLKey := 195;
   SDLK_WORLD_36 : constant SDLKey := 196;
   SDLK_WORLD_37 : constant SDLKey := 197;
   SDLK_WORLD_38 : constant SDLKey := 198;
   SDLK_WORLD_39 : constant SDLKey := 199;
   SDLK_WORLD_40 : constant SDLKey := 200;
   SDLK_WORLD_41 : constant SDLKey := 201;
   SDLK_WORLD_42 : constant SDLKey := 202;
   SDLK_WORLD_43 : constant SDLKey := 203;
   SDLK_WORLD_44 : constant SDLKey := 204;
   SDLK_WORLD_45 : constant SDLKey := 205;
   SDLK_WORLD_46 : constant SDLKey := 206;
   SDLK_WORLD_47 : constant SDLKey := 207;
   SDLK_WORLD_48 : constant SDLKey := 208;
   SDLK_WORLD_49 : constant SDLKey := 209;
   SDLK_WORLD_50 : constant SDLKey := 210;
   SDLK_WORLD_51 : constant SDLKey := 211;
   SDLK_WORLD_52 : constant SDLKey := 212;
   SDLK_WORLD_53 : constant SDLKey := 213;
   SDLK_WORLD_54 : constant SDLKey := 214;
   SDLK_WORLD_55 : constant SDLKey := 215;
   SDLK_WORLD_56 : constant SDLKey := 216;
   SDLK_WORLD_57 : constant SDLKey := 217;
   SDLK_WORLD_58 : constant SDLKey := 218;
   SDLK_WORLD_59 : constant SDLKey := 219;
   SDLK_WORLD_60 : constant SDLKey := 220;
   SDLK_WORLD_61 : constant SDLKey := 221;
   SDLK_WORLD_62 : constant SDLKey := 222;
   SDLK_WORLD_63 : constant SDLKey := 223;
   SDLK_WORLD_64 : constant SDLKey := 224;
   SDLK_WORLD_65 : constant SDLKey := 225;
   SDLK_WORLD_66 : constant SDLKey := 226;
   SDLK_WORLD_67 : constant SDLKey := 227;
   SDLK_WORLD_68 : constant SDLKey := 228;
   SDLK_WORLD_69 : constant SDLKey := 229;
   SDLK_WORLD_70 : constant SDLKey := 230;
   SDLK_WORLD_71 : constant SDLKey := 231;
   SDLK_WORLD_72 : constant SDLKey := 232;
   SDLK_WORLD_73 : constant SDLKey := 233;
   SDLK_WORLD_74 : constant SDLKey := 234;
   SDLK_WORLD_75 : constant SDLKey := 235;
   SDLK_WORLD_76 : constant SDLKey := 236;
   SDLK_WORLD_77 : constant SDLKey := 237;
   SDLK_WORLD_78 : constant SDLKey := 238;
   SDLK_WORLD_79 : constant SDLKey := 239;
   SDLK_WORLD_80 : constant SDLKey := 240;
   SDLK_WORLD_81 : constant SDLKey := 241;
   SDLK_WORLD_82 : constant SDLKey := 242;
   SDLK_WORLD_83 : constant SDLKey := 243;
   SDLK_WORLD_84 : constant SDLKey := 244;
   SDLK_WORLD_85 : constant SDLKey := 245;
   SDLK_WORLD_86 : constant SDLKey := 246;
   SDLK_WORLD_87 : constant SDLKey := 247;
   SDLK_WORLD_88 : constant SDLKey := 248;
   SDLK_WORLD_89 : constant SDLKey := 249;
   SDLK_WORLD_90 : constant SDLKey := 250;
   SDLK_WORLD_91 : constant SDLKey := 251;
   SDLK_WORLD_92 : constant SDLKey := 252;
   SDLK_WORLD_93 : constant SDLKey := 253;
   SDLK_WORLD_94 : constant SDLKey := 254;
   SDLK_WORLD_95 : constant SDLKey := 255;
   SDLK_KP0 : constant SDLKey := 256;
   SDLK_KP1 : constant SDLKey := 257;
   SDLK_KP2 : constant SDLKey := 258;
   SDLK_KP3 : constant SDLKey := 259;
   SDLK_KP4 : constant SDLKey := 260;
   SDLK_KP5 : constant SDLKey := 261;
   SDLK_KP6 : constant SDLKey := 262;
   SDLK_KP7 : constant SDLKey := 263;
   SDLK_KP8 : constant SDLKey := 264;
   SDLK_KP9 : constant SDLKey := 265;
   SDLK_KP_PERIOD : constant SDLKey := 266;
   SDLK_KP_DIVIDE : constant SDLKey := 267;
   SDLK_KP_MULTIPLY : constant SDLKey := 268;
   SDLK_KP_MINUS : constant SDLKey := 269;
   SDLK_KP_PLUS : constant SDLKey := 270;
   SDLK_KP_ENTER : constant SDLKey := 271;
   SDLK_KP_EQUALS : constant SDLKey := 272;
   SDLK_UP : constant SDLKey := 273;
   SDLK_DOWN : constant SDLKey := 274;
   SDLK_RIGHT : constant SDLKey := 275;
   SDLK_LEFT : constant SDLKey := 276;
   SDLK_INSERT : constant SDLKey := 277;
   SDLK_HOME : constant SDLKey := 278;
   SDLK_END : constant SDLKey := 279;
   SDLK_PAGEUP : constant SDLKey := 280;
   SDLK_PAGEDOWN : constant SDLKey := 281;
   SDLK_F1 : constant SDLKey := 282;
   SDLK_F2 : constant SDLKey := 283;
   SDLK_F3 : constant SDLKey := 284;
   SDLK_F4 : constant SDLKey := 285;
   SDLK_F5 : constant SDLKey := 286;
   SDLK_F6 : constant SDLKey := 287;
   SDLK_F7 : constant SDLKey := 288;
   SDLK_F8 : constant SDLKey := 289;
   SDLK_F9 : constant SDLKey := 290;
   SDLK_F10 : constant SDLKey := 291;
   SDLK_F11 : constant SDLKey := 292;
   SDLK_F12 : constant SDLKey := 293;
   SDLK_F13 : constant SDLKey := 294;
   SDLK_F14 : constant SDLKey := 295;
   SDLK_F15 : constant SDLKey := 296;
   SDLK_NUMLOCK : constant SDLKey := 300;
   SDLK_CAPSLOCK : constant SDLKey := 301;
   SDLK_SCROLLOCK : constant SDLKey := 302;
   SDLK_RSHIFT : constant SDLKey := 303;
   SDLK_LSHIFT : constant SDLKey := 304;
   SDLK_RCTRL : constant SDLKey := 305;
   SDLK_LCTRL : constant SDLKey := 306;
   SDLK_RALT : constant SDLKey := 307;
   SDLK_LALT : constant SDLKey := 308;
   SDLK_RMETA : constant SDLKey := 309;
   SDLK_LMETA : constant SDLKey := 310;
   SDLK_LSUPER : constant SDLKey := 311;
   SDLK_RSUPER : constant SDLKey := 312;
   SDLK_MODE : constant SDLKey := 313;
   SDLK_COMPOSE : constant SDLKey := 314;
   SDLK_HELP : constant SDLKey := 315;
   SDLK_PRINT : constant SDLKey := 316;
   SDLK_SYSREQ : constant SDLKey := 317;
   SDLK_BREAK : constant SDLKey := 318;
   SDLK_MENU : constant SDLKey := 319;
   SDLK_POWER : constant SDLKey := 320;
   SDLK_EURO : constant SDLKey := 321;
   SDLK_UNDO : constant SDLKey := 322;
   SDLK_LAST : constant SDLKey := 323;  -- ../include/SDL/SDL_keysym.h:302

   subtype SDLMod is unsigned;
   KMOD_NONE : constant SDLMod := 0;
   KMOD_LSHIFT : constant SDLMod := 1;
   KMOD_RSHIFT : constant SDLMod := 2;
   KMOD_LCTRL : constant SDLMod := 64;
   KMOD_RCTRL : constant SDLMod := 128;
   KMOD_LALT : constant SDLMod := 256;
   KMOD_RALT : constant SDLMod := 512;
   KMOD_LMETA : constant SDLMod := 1024;
   KMOD_RMETA : constant SDLMod := 2048;
   KMOD_NUM : constant SDLMod := 4096;
   KMOD_CAPS : constant SDLMod := 8192;
   KMOD_MODE : constant SDLMod := 16384;
   KMOD_RESERVED : constant SDLMod := 32768;  -- ../include/SDL/SDL_keysym.h:319

end SDL_SDL_keysym_h;
