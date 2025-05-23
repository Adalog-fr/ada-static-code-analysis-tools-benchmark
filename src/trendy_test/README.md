
# trendy_test

Minimum Effort Ada Unit Testing Library

[![Build Status](https://github.com/pyjarrett/trendy_test/actions/workflows/build.yml/badge.svg)](https://github.com/pyjarrett/trendy_test/actions)

## Features

- Minimum setup
- Parallelization of tests
- Randomization of test order to find dependencies between tests
- No generated code or scripts needed

## Example

```ada
with Ada.Text_IO;
with Trendy_Test;
with Trendy_Test.Assertions.Integer_Assertions;
with Trendy_Test.Reports;

use Trendy_Test.Assertions;
use Trendy_Test.Assertions.Integer_Assertions;

procedure My_Test_Main is
begin
    Trendy_Test.Register (My_Tests.All_Tests);
    Trendy_Test.Reports.Print_Basic_Report(Trendy_Test.Run);
end My_Test_Main;

----------------------------------------------------------------

with Trendy_Test;
package My_Tests is
    function All_Tests return Trendy_Test.Test_Group;
end Trendy_Command_Line_Tests;

----------------------------------------------------------------

package body My_Tests is
    procedure Test_Sample (T : in out Trendy_Test.Operation'Class) is
    begin
        -- Don't put anything above here you don't want run during listing/other ops.
        T.Register;
        Assert (T, Some_Expression);
    end Test_Sample;

    procedure Test_Is_Disabled (T : in out Trendy_Test.Operation'Class) is
    begin
        T.Register(Disabled => True);  -- Disabled, don't run this test.
        Assert (T, Some_Expression);
    end Test_Sample;

    procedure Test_Is_Not_Run_In_Parallel (T : in out Trendy_Test.Operation'Class) is
    begin
        T.Register(Parallelize => False);  -- There's some dependency, so don't run it in parallel.
        Assert (T, Some_Expression);
    end Test_Sample;

    function All_Tests return Trendy_Test.Test_Group is
    begin
        return
            (Test_Sample'Access,
             Test_Is_Disabled'Access
            );
    end All_Tests;
end My_Tests;
```

## Example Output

```
D:\dev\ada\trendy_command_line\bin\trendy_command_line_test_main
[ PASS ] shared_pointers_tests.test_multiple_oracles                            0.000105300
[ PASS ] shared_pointers_tests.test_single_oracle                               0.000052700
[ PASS ] trendy_command_line.context_free.tests.test_is_long_option             0.000002300
[ PASS ] trendy_command_line.context_free.tests.test_is_option_terminator       0.000007200
[ PASS ] trendy_command_line.context_free.tests.test_is_short_option            0.000005000
[ PASS ] trendy_command_line.context_free.tests.test_is_short_option_or_group   0.000009200
[ PASS ] trendy_command_line_tests.flags.test_boolean_option_defaults           0.000149400
[ FAIL ] trendy_command_line_tests.flags.test_boolean_option_short_option_group 0.000185600
         Assertion Failed: (Condition false) at D:\dev\ada\trendy_command_line\src\trendy_command_line_tests-flags.adb: 110[D:\dev\ada\trendy_command_line\bin\trendy_command_line_test_main.exe]
0x453530 Trendy_Test.Report_Failure at trendy_test.adb:88
0x4535a4 Trendy_Test.Assert at trendy_test.adb:102
0x425338 Trendy_Command_Line_Tests.Flags.Test_Boolean_Option_Short_Option_Group at trendy_command_line_tests-flags.adb:110
0x451ff1 Trendy_Test.Run_Test at trendy_test.adb:237
0x452d4c Trendy_Test.Run.Parallel_Test_TaskTB at trendy_test.adb:277
0x4601a2 system__tasking__queuing__select_protected_entry_call at ???
[C:\Windows\System32\KERNEL32.DLL]
0x7ffd7d1f7032
[C:\Windows\SYSTEM32\ntdll.dll]
0x7ffd7e08264f

[ FAIL ] trendy_command_line_tests.flags.test_boolean_option_short_options      0.001984800
         Assertion Failed: (Condition false) at D:\dev\ada\trendy_command_line\src\trendy_command_line_tests-flags.adb: 93[D:\dev\ada\trendy_command_line\bin\trendy_command_line_test_main.exe]
0x453530 Trendy_Test.Report_Failure at trendy_test.adb:88
0x4535a4 Trendy_Test.Assert at trendy_test.adb:102
0x425a43 Trendy_Command_Line_Tests.Flags.Test_Boolean_Option_Short_Options at trendy_command_line_tests-flags.adb:93
0x451ff1 Trendy_Test.Run_Test at trendy_test.adb:237
0x452d4c Trendy_Test.Run.Parallel_Test_TaskTB at trendy_test.adb:277
0x4601a2 system__tasking__queuing__select_protected_entry_call at ???
[C:\Windows\System32\KERNEL32.DLL]
0x7ffd7d1f7032
[C:\Windows\SYSTEM32\ntdll.dll]
0x7ffd7e08264f

[ PASS ] trendy_command_line_tests.flags.test_boolean_option_toggles            0.000153600
[ PASS ] trendy_command_line_tests.flags.test_boolean_option_too_many_occurrences
                                                                                0.003517400
[ PASS ] trendy_command_line_tests.test_long_option_with_argument               0.000013000
[ PASS ] trendy_command_line_tests.test_one_operand                             0.000006700
[ PASS ] trendy_command_line_tests.test_short_option_with_argument              0.000193600
Results: Passed:  12 /  14
```