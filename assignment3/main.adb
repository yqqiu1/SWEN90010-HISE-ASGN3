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
   
   Max_Var_Name_length : Integer := 1024;
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
   
   
   function IsNumber (S:String) return Boolean is
   begin
      return (for all I in S'Range => 
                 S(I) >= '0' and S(I) <= '9');
   end IsNumber;
   
   function IsPin (S:String) return Boolean is
   begin
      return S'Length = 4 and IsNumber(S);
   end IsPin;
     
   
   FlagInvalidInput: Boolean:= False;
   
   procedure RaiseInvalidFlag(S:String) is
   begin
      FlagInvalidInput:= True;
      Put_Line(S);
   end;
   
begin
   
   VariableStore.Init(DB);
   MyStack.Init(MS);
   
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
                           if IsNumber(GetToken(2)) then -- check valid var: if is <NUMBER>
                              if L = Unlocked and IsPin(GetToken(2)) then -- check lock state & precondition
                                 
                                 --body
                                 PIN1 := PIN.From_String(GetToken(2));
                                 L    := Locked;
                                 
                              end if; --otherwise do nothing
                           else
                              RaiseInvalidFlag("Invalid Input : Not a number.");
                           end if;
                        else
                           RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                        end if;
                             
                        
                        
                     elsif GetToken(1) = "unlock" then
                           
                        if NumTokens = 2 then -- check valid NumTokens
                           if IsNumber(GetToken(2)) then -- check valid var: if is a <NUMBER>
                              if L = Locked and IsPin(GetToken(2)) then -- check lock state & precondition
                                 
                                 --body
                                 if PIN."="(PIN1,PIN.From_String(GetToken(2))) then 
                                 
                                    L:= Unlocked;
                                 end if;
                                 
                              end if; --otherwise: the pin is not correct, do nothing.
                           else
                              RaiseInvalidFlag("Invalid Input : Not a number.");
                           end if;
                        else
                           RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                        end if;
                        
                        
                        
                     elsif GetToken(1) = "push" then
                        
                        if NumTokens = 2 then -- check valid NumTokens
                           if L = Unlocked then -- check lock state
                              if MyStack.Size(MS) < Max_Stack_Size then -- check precondition
                                 MyStack.Push(MS, StringToInteger.From_String(GetToken(2)));
                              else
                                 Put_Line("Stack Full.");
                              end if;
                           end if; --otherwise: locked state, do nothing
                        else
                           RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                        end if;
                        
                        
                        
                     elsif GetToken(1) = "pop" then
                        
                        if NumTokens = 1 then -- check valid NumTokens
                           if L = Unlocked then -- check lock state
                              if MyStack.Size(MS) > 0 then -- check precondition
                                 declare
                                    outputNumber : Integer;
                                 begin
                                    MyStack.Pop(MS, outputNumber);
                                    Put_Line(outputNumber'Image);
                                 end;
                              else
                                 Put_Line("Stack Empty.");
                              end if;
                           end if; --otherwise: locked state, do nothing
                        else
                           RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                        end if;
                        
                        
                        
                     elsif GetToken(1) = "+" then
                        
                        if NumTokens = 1 then -- check valid NumTokens
                           if L = Unlocked then -- check lock state
                              if MyStack.Size(MS) > 1 then -- check precondition
                                 declare
                                    num1 : Integer;
                                    num2 : Integer;
                                 begin
                                    MyStack.Pop(MS, num1);
                                    MyStack.Pop(MS, num2);
                                    
                                    if (if num1 >= 0 then num2 <= Integer'Last - num1
                                        else num2 >= Integer'First - num1) then
                                          
                                       --body of the operation
                                       MyStack.Push(MS, num1+num2);
                                          
                                    else
                                       Put_Line("Overflow.");
                                    end if;
                                 
                                 end;
                              else
                                 Put_Line("Not enough variables for caculation.");
                              end if;
                           end if; --otherwise: locked state, do nothing
                        else
                           RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                        end if;
                        
                        
                        
                     elsif GetToken(1) = "-" then
                        Put_Line("TO DO: -");
                        
                        
                        
                     elsif GetToken(1) = "*" then
                        Put_Line("TO DO: *");
                        
                        
                        
                     elsif GetToken(1) = "/" then
                        Put_Line("TO DO: /");
                        
                        
                        
                     elsif GetToken(1) = "store" then
                        
                        if NumTokens = 2 then -- check valid NumTokens
                           if GetToken(2)'Length <= Max_Var_Name_length then -- check valid
                              if L = Unlocked then -- check lock state
                                 if MyStack.Size(MS) > 0 then -- check precondition
                                    declare
                                       varName : VariableStore.Variable := VariableStore.From_String(GetToken(2));
                                       varValue: Integer;
                                    begin
                                       MyStack.Pop(MS, varValue);
                                       VariableStore.Put(DB,varName,varValue);
                                    end;
                                 else
                                    Put_Line("Stack Empty.");
                                 end if;
                              end if; --otherwise: locked state, do nothing
                           else 
                              RaiseInvalidFlag("Invalid Input : Variable name too long.");
                           end if;
                        else
                           RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                        end if;
                        
                        
                        
                     elsif GetToken(1) = "load" then
                        
                        if NumTokens = 2 then -- check valid NumTokens
                           if GetToken(2)'Length <= Max_Var_Name_length then -- check valid
                              if L = Unlocked then -- check lock state
                                 declare
                                    varName : VariableStore.Variable := VariableStore.From_String(GetToken(2));
                                    varValue: Integer;
                                 begin
                                    if MyStack.Size(MS) < Max_Stack_Size and VariableStore.Has_Variable(DB,varName) then -- check precondition
                                       
                                       --body
                                       varValue := VariableStore.Get(DB,varName);
                                       MyStack.Push(MS, varValue);
                                         
                                    else
                                       Put_Line("Stack full or No such variable.");
                                    end if;
                                 end;
                              end if; --otherwise: locked state, do nothing
                           else 
                              RaiseInvalidFlag("Invalid Input : Variable name too long.");
                           end if;
                        else
                           RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                        end if;
                        
                        
                        
                     elsif GetToken(1) = "list" then
                        
                        if NumTokens = 1 then -- check valid NumTokens
                           if L = Unlocked then -- check lock state
                              
                              -- body 
                              VariableStore.Print(DB);
                              
                           end if; --otherwise: locked state, do nothing
                        else
                           RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                        end if;
                        
                        
                        
                     elsif GetToken(1) = "remove" then
                        
                        if NumTokens = 2 then -- check valid NumTokens
                           if GetToken(2)'Length <= Max_Var_Name_length then -- check valid
                              if L = Unlocked then -- check lock state
                                 declare
                                    varName : VariableStore.Variable := VariableStore.From_String(GetToken(2));
                                 begin
                                    if VariableStore.Has_Variable(DB,varName) then -- check precondition
                                       
                                       --body
                                       VariableStore.Remove(DB,varName);
                                         
                                    else
                                       Put_Line("No such variable.");
                                    end if;
                                 end;
                              end if; --otherwise: locked state, do nothing
                           else 
                              RaiseInvalidFlag("Invalid Input : Variable name too long.");
                           end if;
                        else
                           RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                        end if;
                        
                        
                        
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
