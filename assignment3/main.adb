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
   
   -- Data Structures
            package Lines is new MyString(Max_MyString_Length => 2048+1);
            InputLine : Lines.MyString;
   
   DB  : VariableStore.Database;
   
   package MyStack is new SimpleStack(512,Integer,0);
   MS  : MyStack.SimpleStack;
   
   
   type LockState is (Locked, Unlocked);
   L :LockState ;
   MasterPin : PIN.PIN ;
   
   
   
   function IsNumber (S:String) return Boolean is
   begin
      return (for all I in S'Range => 
                S(I) >= '0' and S(I) <= '9');
   end IsNumber;
   
   
   function IsPin (S:String) return Boolean is
   begin
      return S'Length = 4 and IsNumber(S);
   end IsPin;
     
   
   
   -- Loop control variable and function
   
   FlagInvalidInput: Boolean:= False;
   
   procedure RaiseInvalidFlag(S:String) is
   begin
      FlagInvalidInput:= True;
      Put_Line(S);
   end;
   
begin
   
   
   VariableStore.Init(DB);
   MyStack.Init(MS);
   
   if (if MyCommandLine.Argument_Count = 1 then 
          MyCommandLine.Argument(1)'Length = 4 
       and (for all I in MyCommandLine.Argument(1)'Range => 
              MyCommandLine.Argument(1)(I) >= '0' 
            and MyCommandLine.Argument(1)(I) <= '9')) then
      
      MasterPin := PIN.From_String(MyCommandLine.Argument(1));
      L         := Locked;
   
         
      while not FlagInvalidInput loop
         declare
               
            T         : MyStringTokeniser.TokenArray(1..3) := (others => (Start => 1, Length => 0));
            NumTokens : Natural;
            OP        : Lines.MyString := Lines.From_String("");
            VAR       : Lines.MyString := Lines.From_String("");
            
           
         begin
               
            if L = Locked then Put("locked>");
            else put("unlocked>");
            end if;
               
            Lines.Get_Line(InputLine);
               
            if Lines.length(InputLine) > 2048 then
                  
               RaiseInvalidFlag("Invalid Input : Get an input line longer than" & Integer'Image(2048) & " characters.");
                  
            else
               MyStringTokeniser.Tokenise(Lines.To_String(InputLine),T,NumTokens);
               
               if NumTokens > 0 then
                  OP  := Lines.Substring(InputLine,T(1).Start,T(1).Start+T(1).Length-1);
               end if; 
               if NumTokens > 1 then
                  VAR := Lines.Substring(InputLine,T(2).Start,T(2).Start+T(2).Length-1);
               end if;
               
               
               
               if NumTokens > 0 then

                  if Lines.To_String(OP) = "lock" then
                     
                     -- check valid                   
                     if (if NumTokens = 2 then IsNumber(Lines.To_String(VAR)) else False) then 
                        
                        -- check precondition
                        if L = Unlocked and IsPin(Lines.To_String(VAR)) then 
                                 
                           --body
                           MasterPin := PIN.From_String(Lines.To_String(VAR));
                           L    := Locked;
                                 
                        end if; 
                        
                     else
                        RaiseInvalidFlag("Invalid Input.");
                     end if;
                     
                        
                        
                  elsif Lines.To_String(OP) = "unlock" then
                     
                     -- check valid 
                     if (if NumTokens = 2 then IsNumber(Lines.To_String(VAR))
                        else False) then 
                        
                        -- check precondition
                        if L = Locked and IsPin(Lines.To_String(VAR)) then 
                                 
                           --body
                           if PIN."="(MasterPin,PIN.From_String(Lines.To_String(VAR))) then 
                              L:= Unlocked;
                           end if;

                        end if; 
                        
                     else
                        RaiseInvalidFlag("Invalid Input.");
                     end if;
                        
                        
                        
                  elsif Lines.To_String(OP) = "push" then
                     
                     -- check valid 
                     if NumTokens = 2 then 
                        
                        -- check precondition
                        if L = Unlocked and MyStack.Size(MS) < 512 then 
                           
                           --body
                           MyStack.Push(MS, StringToInteger.From_String(Lines.To_String(VAR)));
                           
                        else
                           if not (L = Unlocked) then Put_Line("Stack Full.");end if;
                        end if;
                        
                     else
                        RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                     end if;
                        
                     
                        
                  elsif Lines.To_String(OP) = "pop" then
                        
                     -- check valid
                     if NumTokens = 1 then 
                           
                        -- check precondition
                        if L = Unlocked and MyStack.Size(MS) > 0 then
                           declare
                              outputNumber : Integer;
                           begin
                              MyStack.Pop(MS, outputNumber);
                              Put_Line(outputNumber'Image);
                           end;
                        else
                           if not (L = Unlocked) then Put_Line("Stack Empty.");end if; 
                        end if;
                     else
                        RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                     end if;
                        
                     
                        
                  elsif Lines.To_String(OP) = "+" then
                     
                     -- check valid NumTokens
                     if NumTokens = 1 then 
                        
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
                                          
                                    --body
                                    MyStack.Push(MS, num1+num2);
                                          
                                 else
                                    Put_Line("Overflow.");
                                 end if;
                                 
                              end;
                           else
                              Put_Line("Not enough variables for caculation.");
                           end if;
                        end if; 
                     else
                        RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                     end if;
                        
                        
                        
                  elsif Lines.To_String(OP) = "-" then
                        
                     if NumTokens = 1 then -- check valid NumTokens
                        if L = Unlocked then -- check lock state
                           if MyStack.Size(MS) > 1 then -- check precondition
                              declare
                                 num1 : Integer;
                                 num2 : Integer;
                              begin
                                 MyStack.Pop(MS, num1);
                                 MyStack.Pop(MS, num2);
                                    
                                 if (if num2 >= 0 then num1 >= Integer'First + num2
                                     else num1 <= Integer'Last + num2) then
                                          
                                    --body of the operation
                                    MyStack.Push(MS, num1 - num2);
                                          
                                 else
                                    Put_Line("Overflow.");
                                 end if;
                                 
                              end;
                           else
                              Put_Line("Not enough variables for caculation.");
                           end if;
                        end if; 
                     else
                        RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                     end if;
                        
                        
                        
                  elsif Lines.To_String(OP) = "*" then
                        
                     if NumTokens = 1 then -- check valid NumTokens
                        if L = Unlocked then -- check lock state
                           if MyStack.Size(MS) > 1 then -- check precondition
                              declare
                                 num1 : Integer;
                                 num2 : Integer;
                              begin
                                 MyStack.Pop(MS, num1);
                                 MyStack.Pop(MS, num2);
                                    
                                 if (if num1 = 0 then True
                                     elsif num1 = -1 then num2 /= Integer'First
                                     elsif num1 > 0 then ((num2 >= Integer'First / num1) and (num2 <= Integer'Last / num1))
                                     else ((num2 >= Integer'Last / num1) and (num2 <= Integer'First / num1))) then
                                       
                                    --body of the operation
                                    MyStack.Push(MS, num1 * num2);
                                          
                                 else
                                    Put_Line("Overflow.");
                                 end if;
                                 
                              end;
                           else
                              Put_Line("Not enough variables for caculation.");
                           end if;
                        end if; 
                     else
                        RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                     end if;
                        
                        
                        
                  elsif Lines.To_String(OP) = "/" then
                        
                     if NumTokens = 1 then -- check valid NumTokens
                        if L = Unlocked then -- check lock state
                           if MyStack.Size(MS) > 1 then -- check precondition
                              declare
                                 num1 : Integer;
                                 num2 : Integer;
                              begin
                                 MyStack.Pop(MS, num1);
                                 MyStack.Pop(MS, num2);
                                    
                                 if (num2 /= 0 and
                                       (if num1 = Integer'First then num2/=-1)) then
                                          
                                    --body of the operation
                                    MyStack.Push(MS, num1 / num2);
                                          
                                 else
                                    Put_Line("Overflow.");
                                 end if;
                                 
                              end;
                           else
                              Put_Line("Not enough variables for caculation.");
                           end if;
                        end if; 
                     else
                        RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                     end if;
                        
                        
                        
                  elsif Lines.To_String(OP) = "store" then
                        
                     if NumTokens = 2 then -- check valid NumTokens
                        if Lines.To_String(VAR)'Length <= 1024 then -- check valid
                           if L = Unlocked then -- check lock state
                              if MyStack.Size(MS) > 0 then -- check precondition
                                 declare
                                    varName : VariableStore.Variable := VariableStore.From_String(Lines.To_String(VAR));
                                    varValue: Integer;
                                 begin
                                    MyStack.Pop(MS, varValue);
                                    VariableStore.Put(DB,varName,varValue);
                                 end;
                              else
                                 Put_Line("Stack Empty.");
                              end if;
                           end if; 
                        else 
                           RaiseInvalidFlag("Invalid Input : Variable name too long.");
                        end if;
                     else
                        RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                     end if;
                        
                        
                        
                  elsif Lines.To_String(OP) = "load" then
                        
                     if NumTokens = 2 then -- check valid NumTokens
                        if Lines.To_String(VAR)'Length <= 1024 then -- check valid
                           if L = Unlocked then -- check lock state
                              declare
                                 varName : VariableStore.Variable := VariableStore.From_String(Lines.To_String(VAR));
                                 varValue: Integer;
                              begin
                                 if MyStack.Size(MS) < 512 and VariableStore.Has_Variable(DB,varName) then -- check precondition
                                       
                                    --body
                                    varValue := VariableStore.Get(DB,varName);
                                    MyStack.Push(MS, varValue);
                                         
                                 else
                                    Put_Line("Stack full or No such variable.");
                                 end if;
                              end;
                           end if; 
                        else 
                           RaiseInvalidFlag("Invalid Input : Variable name too long.");
                        end if;
                     else
                        RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                     end if;
                        
                        
                        
                  elsif Lines.To_String(OP) = "list" then
                        
                     if NumTokens = 1 then -- check valid NumTokens
                        if L = Unlocked then -- check lock state
                              
                           -- body 
                           VariableStore.Print(DB);
                              
                        end if; 
                     else
                        RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                     end if;
                        
                        
                        
                  elsif Lines.To_String(OP) = "remove" then
                        
                     if NumTokens = 2 then -- check valid NumTokens
                        if Lines.To_String(VAR)'Length <= 1024 then -- check valid
                           if L = Unlocked then -- check lock state
                              declare
                                 varName : VariableStore.Variable := VariableStore.From_String(Lines.To_String(VAR));
                              begin
                                 if VariableStore.Has_Variable(DB,varName) then -- check precondition
                                       
                                    --body
                                    VariableStore.Remove(DB,varName);
                                         
                                 else
                                    Put_Line("No such variable.");
                                 end if;
                              end;
                           end if;
                        else 
                           RaiseInvalidFlag("Invalid Input : Variable name too long.");
                        end if;
                     else
                        RaiseInvalidFlag("Invalid Input : Wrong number of tokens.");
                     end if;
                  else
                     RaiseInvalidFlag("Invalid Input : No such command: " & Lines.To_String(OP) & ".");
                  end if;
               end if;
            end if;
         end;
      end loop;
       
   end if;
end Main;

