pragma SPARK_Mode (On);

with StringToInteger;
with VariableStore;
with MyCommandLine;
with MyString;
with MyStringTokeniser;
with PIN;
with SimpleStack;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Task_Identification;  use Ada.Task_Identification;
with Ada.Long_Long_Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Main is
   DB  : VariableStore.Database;
   package MyStack is new SimpleStack(512,Integer,0);
   MS  : MyStack.SimpleStack;
   
   --V1 : VariableStore.Variable := VariableStore.From_String("Var1");
   
   type LockState is (Locked, Unlocked);
   L :LockState := Unlocked;
   PIN1 : PIN.PIN := PIN.From_String("1234");
   PIN2  : PIN.PIN := PIN.From_String("1234");
   package Lines is new MyString(Max_MyString_Length => 2049);
   S  : Lines.MyString;
   
   FlagValidInput: Boolean:= True;
   
begin
   
   if MyCommandLine.Argument_Count = 1 then
      if MyCommandLine.Argument(1)'Length = 4 
        and (for all I in MyCommandLine.Argument(1)'Range => 
                 MyCommandLine.Argument(1)(I) >= '0' 
             and MyCommandLine.Argument(1)(I) <= '9') then
      
         PIN1 := PIN.From_String(MyCommandLine.Argument(1));
         L    := Locked;
   
         while FlagValidInput loop
            declare
               T         : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
               NumTokens : Natural;
            begin
               if L = Locked then Put("locked>");
               else put("unlocked>");
               end if;
               Lines.Get_Line(S);
               if Lines.length(S) > 2048 then
                  Put_Line("Invalid Input : Get an input line longer than 2048 characters.");
                  FlagValidInput := False;
               else
                  MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
                  if NumTokens > 0 then
                     declare
                        OP  : String := Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1));
                     begin
                        if OP = "lock" then
                           
                           -- for each 1 
                           -- check lock state
                           -- check num of tokens
                           -- check pre conditions for each command
                           -- body
                           
                           Put_Line("TO DO: lock");
                        elsif OP = "unlock" then
                           Put_Line("TO DO: unlock");
                        elsif OP = "push" then
                           Put_Line("TO DO: push");
                        elsif OP = "pop" then
                           Put_Line("TO DO: pop");
                        elsif OP = "+" then
                           Put_Line("TO DO: +");
                        elsif OP = "-" then
                           Put_Line("TO DO: -");
                        elsif OP = "*" then
                           Put_Line("TO DO: *");
                        elsif OP = "/" then
                           Put_Line("TO DO: /");
                        elsif OP = "store" then
                           Put_Line("TO DO: store");
                        elsif OP = "load" then
                           Put_Line("TO DO: load");
                        elsif OP = "list" then
                           Put_Line("TO DO: list");
                        elsif OP = "remove" then
                           Put_Line("TO DO: remove");
                        else
                           Put("Invalid Input : No such command: ");Put(OP);Put_Line(".");
                           FlagValidInput := False;
                        end if;
                     end;
                  end if;
               end if;
            end;
         end loop;
      end if;
   end if;
end Main;
