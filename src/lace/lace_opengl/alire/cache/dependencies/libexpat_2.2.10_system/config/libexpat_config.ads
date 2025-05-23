--  Configuration for libexpat generated by Alire
pragma Restrictions (No_Elaboration_Code);
pragma Style_Checks (Off);

package Libexpat_Config is
   pragma Pure;

   Crate_Version : constant String := "2.2.10";
   Crate_Name : constant String := "libexpat";

   Alire_Host_OS : constant String := "linux";

   Alire_Host_Arch : constant String := "x86_64";

   Alire_Host_Distro : constant String := "debian";

   type Build_Profile_Kind is (release, validation, development);
   Build_Profile : constant Build_Profile_Kind := release;

end Libexpat_Config;
