with Ada.Command_Line;
with Ada.Text_IO;
with Elf;

procedure readelf
is
   package CLI renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;

begin

   if CLI.Argument_Count = 0 then
      TIO.Put_Line ("usage: readelf <path-to-file>");
      return;
   end if;

   Elf.print_elf_information (CLI.Argument (1));

end readelf;
