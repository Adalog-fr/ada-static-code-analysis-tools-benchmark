# What is Mage

Mage, the Mini Ada Game Engine is a mini framework to quickly build game and small interactive apps in Ada.

# Use Mage

```
alr with mage
```

and in your code

`draw_circle.adb`
```
with Mage;       use Mage;
with Mage.Draw;  use Mage.Draw;
with Mage.Event; use Mage.Event;

procedure Draw_Circle is
   W : Window_ID := Create_Window (800, 600, "Hello");
   C : Canvas_ID := Get_Canvas (W);
begin

   while not Is_Killed loop
      Draw_Circle (C, (400, 300), 300, Red);

      Handle_Events (W);
      delay 0.1;
   end loop;
end Draw_Circle;
```

# Changelog

## v0.5.0

Zoom Factor for the 3D API

## v0.4.1

More consistent API, with Positive and Float_Pos being used

## v0.4.0

Rename GPR library type option

## v0.3.2

Fix windows portability
Window start centered
Add README.md

## v0.3.0

Add `Mage.Input`, `Mage.Log`, `Mage.Event`
Add blue hat moving example
Add moving circle example

## v0.2.0

New API
Rename project to Mage

## v0.1.0

Initial release
