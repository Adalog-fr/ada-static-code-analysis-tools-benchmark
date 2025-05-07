
# Geographic Coordinates calculation for determining the Destination Location or Distance & Bearing

This project/library helps in performing the calculations between geographic coordinates using the `Ada` Programming Language:
1. `Distance_Bearing` - Function `Distance_Bearing` returns the distance and bearing between two geographic locations (home and destination).
2. `Destination_Location` - Function `Destination_Location` returns the destination's location from the home location by using the distance and bearing between two locations.

## Usage
For including the `geo_coords` library in your alire project:
```
alr with geo_coords
```

After this you can include the library in the code as below:
```
with  Geo_Coords_Types; use  Geo_Coords_Types;
with  Geo_Coords_Utilities; use  Geo_Coords_Utilities;
with  Geo_Coords; use  Geo_Coords;
```

## Example

The below example is from one of the examples included in this repository:
```
with Geo_Coords_Types; use Geo_Coords_Types;
with Geo_Coords_Utilities; use Geo_Coords_Utilities;
with Geo_Coords; use Geo_Coords;

procedure Distance_Bearing_Calculation is
   Home_Loc, Dest_Loc : Geo_Location;
   Destination_Bearing : Distance_Bearing_type;
begin
   Home_Loc := (Latitude => 51.502310,
                Longitude => -0.097335);

   Dest_Loc := (Latitude => 51.502398,
                Longitude => -0.095866);

   Destination_Bearing := Distance_Bearing (Home => Home_Loc,
                                            Destination => Dest_Loc);

   Put_Distance_Bearing (Destination_Bearing);

end Distance_Bearing_Calculation;
```

### Output

```
ⓘ Building distance_bearing_calculation/distance_bearing_calculation.gpr...
Compile
   [Ada]          distance_bearing_calculation.adb
   [Ada]          geo_coords_config.ads
   [Ada]          geo_coords.adb
   [Ada]          geo_coords_types.ads
   [Ada]          geo_coords_utilities.adb
Build Libraries
   [gprlib]       Geo_Coords.lexch
   [archive]      libGeo_Coords.a
   [index]        libGeo_Coords.a
Bind
   [gprbind]      distance_bearing_calculation.bexch
   [Ada]          distance_bearing_calculation.ali
Link
   [link]         distance_bearing_calculation.adb
Build finished successfully in 6.57 seconds.
Distance = 102.14937003520048 (Metres)
Bearing = 84.50248476050882 (Degrees)
```

## Project Structure

```
.
├── alire
│   ├── alire.lock
│   └── config.toml
├── alire.toml
├── config
│   ├── geo_coords_config.ads
│   ├── geo_coords_config.gpr
│   └── geo_coords_config.h
├── examples
│   ├── distance_bearing_calculation
│   │   ├── alire
│   │   │   ├── alire.lock
│   │   │   └── config.toml
│   │   ├── alire.toml
│   │   ├── bin
│   │   ├── config
│   │   │   ├── distance_bearing_calculation_config.ads
│   │   │   ├── distance_bearing_calculation_config.gpr
│   │   │   └── distance_bearing_calculation_config.h
│   │   ├── distance_bearing_calculation.gpr
│   │   ├── obj
│   │   │   └── development
│   │   ├── share
│   │   │   └── distance_bearing_calculation
│   │   └── src
│   │       └── distance_bearing_calculation.adb
│   ├── geo_coords_calculation
│   │   ├── alire
│   │   │   ├── alire.lock
│   │   │   └── config.toml
│   │   ├── alire.toml
│   │   ├── bin
│   │   ├── config
│   │   │   ├── geo_coords_calculation_config.ads
│   │   │   ├── geo_coords_calculation_config.gpr
│   │   │   └── geo_coords_calculation_config.h
│   │   ├── geo_coords_calculation.gpr
│   │   ├── obj
│   │   │   └── development
│   │   ├── share
│   │   │   └── geo_coords_calculation
│   │   └── src
│   │       └── geo_coords_calculation.adb
│   └── lat_lon_calculation
│       ├── alire
│       │   ├── alire.lock
│       │   └── config.toml
│       ├── alire.toml
│       ├── bin
│       ├── config
│       │   ├── lat_lon_calculation_config.ads
│       │   ├── lat_lon_calculation_config.gpr
│       │   └── lat_lon_calculation_config.h
│       ├── lat_lon_calculation.gpr
│       ├── obj
│       │   └── development
│       ├── share
│       │   └── lat_lon_calculation
│       └── src
│           └── lat_lon_calculation.adb
├── geo_coords.gpr
├── lib
├── LICENSE
├── obj
│   ├── development
│   └── release
├── README.md
├── share
│   └── geo_coords
└── src
    ├── geo_coords.adb
    ├── geo_coords.ads
    ├── geo_coords_types.ads
    ├── geo_coords_utilities.adb
    └── geo_coords_utilities.ads
```

```
ⓘ Note: Above structure is before building the project!
```

## Additional Details

The following repository is archived because it is the binary version of the Example `geo_coords_calculation` added in this project:
1. https://github.com/TechnicalVillager/geo-coords-calculation

## References
1. https://github.com/TechnicalVillager/geo-coords-calculation
2. https://github.com/TechnicalVillager/distance-bearing-calculation
3. https://www.movable-type.co.uk/scripts/latlong.html