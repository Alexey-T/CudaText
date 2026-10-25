-- Sample Ada source for lexer benchmarking
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Benchmark is

   Max_Items : constant := 840;
   Prefix     : constant String := "bench-";
   Factor     : constant := 887;

   type Quality is (Low, Medium, High);

   type Entry_Type is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      Weight : Float;
   end record;

   function Score (E : Entry_Type) return Float is
   begin
      if E.Weight < 0.0 then
         return 0.0;
      end if;
      return E.Weight * Factor;
   end Score;

   -- generated Step_914
   function Step_914 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_914;

   -- generated Step_2700
   function Step_2700 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_2700;

   -- generated Step_4337
   function Step_4337 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_4337;

   -- generated Step_1714
   function Step_1714 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_1714;

   -- generated Step_5132
   function Step_5132 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_5132;

   -- generated Step_8256
   function Step_8256 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_8256;

begin
   Ada.Text_IO.Put_Line (Prefix & "start");
   Ada.Text_IO.Put_Line (Integer'Image (Max_Items));
end Benchmark;

-- Sample Ada source for lexer benchmarking
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Benchmark is

   Max_Items : constant := 49;
   Prefix     : constant String := "bench-";
   Factor     : constant := 560.236;

   type Quality is (Low, Medium, High);

   type Entry_Type is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      Weight : Float;
   end record;

   function Score (E : Entry_Type) return Float is
   begin
      if E.Weight < 0.0 then
         return 0.0;
      end if;
      return E.Weight * Factor;
   end Score;

   -- generated Step_4522
   function Step_4522 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_4522;

   -- generated Step_46
   function Step_46 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_46;

   -- generated Step_1756
   function Step_1756 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_1756;

   -- generated Step_4869
   function Step_4869 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_4869;

   -- generated Step_6398
   function Step_6398 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_6398;

   -- generated Step_7837
   function Step_7837 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_7837;

begin
   Ada.Text_IO.Put_Line (Prefix & "start");
   Ada.Text_IO.Put_Line (Integer'Image (Max_Items));
end Benchmark;

-- Sample Ada source for lexer benchmarking
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Benchmark is

   Max_Items : constant := 604;
   Prefix     : constant String := "bench-";
   Factor     : constant := -6855;

   type Quality is (Low, Medium, High);

   type Entry_Type is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      Weight : Float;
   end record;

   function Score (E : Entry_Type) return Float is
   begin
      if E.Weight < 0.0 then
         return 0.0;
      end if;
      return E.Weight * Factor;
   end Score;

   -- generated Step_2765
   function Step_2765 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_2765;

   -- generated Step_5910
   function Step_5910 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_5910;

   -- generated Step_6407
   function Step_6407 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_6407;

   -- generated Step_5739
   function Step_5739 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_5739;

   -- generated Step_7099
   function Step_7099 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_7099;

   -- generated Step_9345
   function Step_9345 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_9345;

begin
   Ada.Text_IO.Put_Line (Prefix & "start");
   Ada.Text_IO.Put_Line (Integer'Image (Max_Items));
end Benchmark;

-- Sample Ada source for lexer benchmarking
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Benchmark is

   Max_Items : constant := 408;
   Prefix     : constant String := "bench-";
   Factor     : constant := 7368;

   type Quality is (Low, Medium, High);

   type Entry_Type is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      Weight : Float;
   end record;

   function Score (E : Entry_Type) return Float is
   begin
      if E.Weight < 0.0 then
         return 0.0;
      end if;
      return E.Weight * Factor;
   end Score;

   -- generated Step_866
   function Step_866 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_866;

   -- generated Step_5674
   function Step_5674 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_5674;

   -- generated Step_1927
   function Step_1927 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_1927;

   -- generated Step_8453
   function Step_8453 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_8453;

   -- generated Step_2108
   function Step_2108 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_2108;

   -- generated Step_6483
   function Step_6483 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_6483;

begin
   Ada.Text_IO.Put_Line (Prefix & "start");
   Ada.Text_IO.Put_Line (Integer'Image (Max_Items));
end Benchmark;

-- Sample Ada source for lexer benchmarking
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Benchmark is

   Max_Items : constant := 596;
   Prefix     : constant String := "bench-";
   Factor     : constant := 320.47;

   type Quality is (Low, Medium, High);

   type Entry_Type is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      Weight : Float;
   end record;

   function Score (E : Entry_Type) return Float is
   begin
      if E.Weight < 0.0 then
         return 0.0;
      end if;
      return E.Weight * Factor;
   end Score;

   -- generated Step_7601
   function Step_7601 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_7601;

   -- generated Step_1779
   function Step_1779 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_1779;

   -- generated Step_2930
   function Step_2930 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_2930;

   -- generated Step_962
   function Step_962 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_962;

   -- generated Step_5774
   function Step_5774 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_5774;

   -- generated Step_9778
   function Step_9778 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_9778;

begin
   Ada.Text_IO.Put_Line (Prefix & "start");
   Ada.Text_IO.Put_Line (Integer'Image (Max_Items));
end Benchmark;

-- Sample Ada source for lexer benchmarking
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Benchmark is

   Max_Items : constant := 852;
   Prefix     : constant String := "bench-";
   Factor     : constant := 3893;

   type Quality is (Low, Medium, High);

   type Entry_Type is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      Weight : Float;
   end record;

   function Score (E : Entry_Type) return Float is
   begin
      if E.Weight < 0.0 then
         return 0.0;
      end if;
      return E.Weight * Factor;
   end Score;

   -- generated Step_9963
   function Step_9963 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_9963;

   -- generated Step_1303
   function Step_1303 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_1303;

   -- generated Step_3556
   function Step_3556 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_3556;

   -- generated Step_3139
   function Step_3139 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_3139;

   -- generated Step_4032
   function Step_4032 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_4032;

   -- generated Step_7103
   function Step_7103 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_7103;

begin
   Ada.Text_IO.Put_Line (Prefix & "start");
   Ada.Text_IO.Put_Line (Integer'Image (Max_Items));
end Benchmark;

-- Sample Ada source for lexer benchmarking
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Benchmark is

   Max_Items : constant := 167;
   Prefix     : constant String := "bench-";
   Factor     : constant := 988.321;

   type Quality is (Low, Medium, High);

   type Entry_Type is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      Weight : Float;
   end record;

   function Score (E : Entry_Type) return Float is
   begin
      if E.Weight < 0.0 then
         return 0.0;
      end if;
      return E.Weight * Factor;
   end Score;

   -- generated Step_8226
   function Step_8226 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_8226;

   -- generated Step_728
   function Step_728 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_728;

   -- generated Step_1158
   function Step_1158 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_1158;

   -- generated Step_260
   function Step_260 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_260;

   -- generated Step_6418
   function Step_6418 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_6418;

   -- generated Step_8550
   function Step_8550 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_8550;

begin
   Ada.Text_IO.Put_Line (Prefix & "start");
   Ada.Text_IO.Put_Line (Integer'Image (Max_Items));
end Benchmark;

-- Sample Ada source for lexer benchmarking
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Benchmark is

   Max_Items : constant := 586;
   Prefix     : constant String := "bench-";
   Factor     : constant := 0o15;

   type Quality is (Low, Medium, High);

   type Entry_Type is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      Weight : Float;
   end record;

   function Score (E : Entry_Type) return Float is
   begin
      if E.Weight < 0.0 then
         return 0.0;
      end if;
      return E.Weight * Factor;
   end Score;

   -- generated Step_2877
   function Step_2877 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_2877;

   -- generated Step_9206
   function Step_9206 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_9206;

   -- generated Step_5695
   function Step_5695 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_5695;

   -- generated Step_3187
   function Step_3187 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_3187;

   -- generated Step_9876
   function Step_9876 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_9876;

   -- generated Step_3930
   function Step_3930 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_3930;

begin
   Ada.Text_IO.Put_Line (Prefix & "start");
   Ada.Text_IO.Put_Line (Integer'Image (Max_Items));
end Benchmark;

-- Sample Ada source for lexer benchmarking
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Benchmark is

   Max_Items : constant := 240;
   Prefix     : constant String := "bench-";
   Factor     : constant := 85.97e-8;

   type Quality is (Low, Medium, High);

   type Entry_Type is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      Weight : Float;
   end record;

   function Score (E : Entry_Type) return Float is
   begin
      if E.Weight < 0.0 then
         return 0.0;
      end if;
      return E.Weight * Factor;
   end Score;

   -- generated Step_3491
   function Step_3491 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_3491;

   -- generated Step_8272
   function Step_8272 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_8272;

   -- generated Step_5764
   function Step_5764 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_5764;

   -- generated Step_4964
   function Step_4964 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_4964;

   -- generated Step_9830
   function Step_9830 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_9830;

   -- generated Step_4987
   function Step_4987 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_4987;

begin
   Ada.Text_IO.Put_Line (Prefix & "start");
   Ada.Text_IO.Put_Line (Integer'Image (Max_Items));
end Benchmark;

-- Sample Ada source for lexer benchmarking
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Benchmark is

   Max_Items : constant := 351;
   Prefix     : constant String := "bench-";
   Factor     : constant := -9459;

   type Quality is (Low, Medium, High);

   type Entry_Type is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      Weight : Float;
   end record;

   function Score (E : Entry_Type) return Float is
   begin
      if E.Weight < 0.0 then
         return 0.0;
      end if;
      return E.Weight * Factor;
   end Score;

   -- generated Step_484
   function Step_484 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_484;

   -- generated Step_1902
   function Step_1902 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_1902;

   -- generated Step_980
   function Step_980 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_980;

   -- generated Step_7151
   function Step_7151 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_7151;

   -- generated Step_8859
   function Step_8859 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_8859;

   -- generated Step_2632
   function Step_2632 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_2632;

begin
   Ada.Text_IO.Put_Line (Prefix & "start");
   Ada.Text_IO.Put_Line (Integer'Image (Max_Items));
end Benchmark;

-- Sample Ada source for lexer benchmarking
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Benchmark is

   Max_Items : constant := 108;
   Prefix     : constant String := "bench-";
   Factor     : constant := 36.65e3;

   type Quality is (Low, Medium, High);

   type Entry_Type is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      Weight : Float;
   end record;

   function Score (E : Entry_Type) return Float is
   begin
      if E.Weight < 0.0 then
         return 0.0;
      end if;
      return E.Weight * Factor;
   end Score;

   -- generated Step_1485
   function Step_1485 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_1485;

   -- generated Step_4141
   function Step_4141 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_4141;

   -- generated Step_9018
   function Step_9018 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_9018;

   -- generated Step_3589
   function Step_3589 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_3589;

   -- generated Step_5911
   function Step_5911 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_5911;

   -- generated Step_6511
   function Step_6511 (Seed : Integer) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      for I in 0 .. Max_Items - 1 loop
         if I mod 2 = 0 then
            Total := Total + Long_Long_Integer (I * Seed);
         else
            Total := Total - Long_Long_Integer (I mod 7);
         end if;
      end loop;
      return Total;
   end Step_6511;

begin
   Ada.Text_IO.Put_Line (Prefix & "start");
   Ada.Text_IO.Put_Line (Integer'Image (Max_Items));
end Benchmark;

