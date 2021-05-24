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
   
   
   Max_Stack_Size : Integer := 512;
   package MyStack is new SimpleStack(Max_Stack_Size,Integer,0);
   MS  : MyStack.SimpleStack;
   
   
   Max_Command_Length : Integer := 2048;
   package Lines is new MyString(Max_MyString_Length => Max_Command_Length+1);
   S  : Lines.MyString;
   
   
   
   type LockState is (Locked, Unlocked);
   L :LockState := Unlocked;
   PIN1 : PIN.PIN := PIN.From_String("1234");
   
   function IsPin (S:String) return Boolean is
   begin
      return S'Length = 4 
        and (for all I in S'Range => 
                 S(I) >= '0' and S(I) <= '9');
   end IsPin;
     
   
   FlagInvalidInput: Boolean:= False;
   
   procedure RaiseInvalidFlag(S:String) is
   begin
      FlagInvalidInput:= True;
      Put_Line(S);
   end;
   
begin
   
   if MyCommandLine.Argument_Count = 1 then
      if IsPin(MyCommandLine.Argument(1)) then
      
         PIN1 := PIN.From_String(MyCommandLine.Argument(1));
         L    := Locked;
   
         
         
         while not FlagInvalidInput loop
            declare
               
               T         : MyStringTokeniser.TokenArray(1..3) := (others => (Start => 1, Length => 0));
               NumTokens : Natural;
               
               function GetToken ( N:Integer ) return String is
               begin
                  MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
                  if N<=NumTokens then
                     return Lines.To_String(Lines.Substring(S,T(N).Start,T(N).Start+T(N).Length-1));
                  else return "";
                  end if;
               end GetToken;
               
            begin
               
               if L = Locked then Put("locked>");
               else put("unlocked>");
               end if;
               
               Lines.Get_Line(S);
               
               if Lines.length(S) > Max_Command_Length then
                  
                  RaiseInvalidFlag("Invalid Input : Get an input line longer than" & Integer'Image(Max_Command_Length) & " characters.");
                  
               else
                  
                  MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
                  
                  if NumTokens > 0 then
                     -- for each operation 
                     -- check valid (numtokens and variable type)
                     -- check lock state
                     -- check operation pre condition
                     -- body
                        
                     if GetToken(1) = "lock" then
                                                   
                        if NumTokens = 2 then -- check valid NumTokens
                           if IsPin(GetToken(2)) then -- check valid var: if is a pin
                              if L = Unlocked then -- check lock state & precondition
                                 
                                 --body
                                 PIN1 := PIN.From_String(GetToken(2));
                                 L    := Locked;
                                 
                              end if; --otherwise do nothing
                           else
                              RaiseInvalidFlag("Invalid Input : Not a pin.");
                           end if;
                        else
                           RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                        end if;
                             
                        
                        
                     elsif GetToken(1) = "unlock" then
                           
                        if NumTokens = 2 then -- check valid NumTokens
                           if IsPin(GetToken(2)) then -- check valid var: if is a pin 
                              if L = Locked and 
                                PIN."="(PIN1,PIN.From_String(GetToken(2)))  then -- check lock state & precondition
                                 
                                 --body
                                 L:= Unlocked;
                                 
                              end if; --otherwise: the pin is not correct, do nothing.
                           else
                              RaiseInvalidFlag("Invalid Input : Not a pin.");
                           end if;
                        else
                           RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                        end if;
                        
                        
                        
                     elsif GetToken(1) = "push" then
                        Put_Line("TO DO: push");
                        
                        
                        
                     elsif GetToken(1) = "pop" then
                        Put_Line("TO DO: pop");
                        
                        
                        
                     elsif GetToken(1) = "+" then
                        Put_Line("TO DO: +");
                        
                        
                        
                     elsif GetToken(1) = "-" then
                        Put_Line("TO DO: -");
                        
                        
                        
                     elsif GetToken(1) = "*" then
                        Put_Line("TO DO: *");
                        
                        
                        
                     elsif GetToken(1) = "/" then
                        Put_Line("TO DO: /");
                        
                        
                        
                     elsif GetToken(1) = "store" then
                        Put_Line("TO DO: store");
                        
                        
                        
                     elsif GetToken(1) = "load" then
                        Put_Line("TO DO: load");
                        
                        
                        
                     elsif GetToken(1) = "list" then
                        Put_Line("TO DO: list");
                        
                        
                        
                     elsif GetToken(1) = "remove" then
                        Put_Line("TO DO: remove");
                        
                        
                        
                     else
                        RaiseInvalidFlag("Invalid Input : No such command: " & getToken(1) & ".");
                     end if;
                  end if;
               end if;
            end;
         end loop;
      end if;
   end if;
end Main;
