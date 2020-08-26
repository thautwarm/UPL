const mlfs_Prim =
 Int16(0)
const var"|b|Prim.op_Element" =
 u -> x -> x[u + 1]
const var"|d|Prim.op_Make" =
 function (var"|f|Prim.f" :: Any)
  function (var"|h|Prim._" :: String)
   var"|f|Prim.f"
  end
 end
const var"|j|Prim.make" =
 var"|d|Prim.op_Make"
const var"|n|Prim.Eq" =
 "|l|Prim.Eq" :: String
const var"|p|Prim.__get_equal" =
 (u -> x -> x[u + 1])(
  UInt64(0) :: UInt64)
const var"|r|Prim.__make_Eq" =
 begin
  const var"|t|Prim.__make_Eq" =
   function (var"|v|Prim.equal" :: Function)
    (var"|v|Prim.equal",)
   end
  var"|t|Prim.__make_Eq"
 end
const var"|x|Prim.eq" =
 function (var"|z|Prim.eq" :: Any)
  function (var"|bb|Prim.lhs" :: Any)
   function (var"|db|Prim.rhs" :: Any)
    (((((var"|b|Prim.op_Element")(
         UInt64(0) :: UInt64))(
        var"|z|Prim.eq"))(
       var"|bb|Prim.lhs"))(
      var"|db|Prim.rhs")):: Bool
   end
  end
 end
const var"|fb|Prim.eq_i64" =
 (((var"|d|Prim.op_Make")(
    var"|r|Prim.__make_Eq"))(
   "|l|Prim.Eq" :: String))(
  
    let @inline eq(x::Int64) = (y::Int64) -> x === y; eq end
)
const var"|hb|Prim.eq_f64" =
 (((var"|d|Prim.op_Make")(
    var"|r|Prim.__make_Eq"))(
   "|l|Prim.Eq" :: String))(
  
    let @inline eq(x::Float64) = (y::Float64) -> x === y; eq end
)
const var"|jb|Prim.println" =
 println
const var"|lb|Prim.Eq" =
 function (_::Any)
  var"|n|Prim.Eq"
 end
const var"|nb|Prim.__get_equal" =
 function (_::Any)
  var"|p|Prim.__get_equal"
 end
const var"|pb|Prim.__make_Eq" =
 function (_::Any)
  var"|r|Prim.__make_Eq"
 end
const var"|rb|Prim.eq" =
 function (_::Any)
  var"|x|Prim.eq"
 end
const var"|tb|Prim.eq_f64" =
 function (_::Any)
  var"|hb|Prim.eq_f64"
 end
const var"|vb|Prim.eq_i64" =
 function (_::Any)
  var"|fb|Prim.eq_i64"
 end
const var"|xb|Prim.make" =
 function (_::Any)
  var"|j|Prim.make"
 end
const var"|zb|Prim.op_Element" =
 function (_::Any)
  var"|b|Prim.op_Element"
 end
const var"|bc|Prim.op_Make" =
 function (_::Any)
  var"|d|Prim.op_Make"
 end
const var"|dc|Prim.println" =
 function (_::Any)
  var"|jb|Prim.println"
 end
const mlfs_TestPrim =
 Int16(0)
const var"|d|TestPrim.open" =
 mlfs_Prim
const var"|f|TestPrim.println" =
 begin
  var"|d|TestPrim.open"
  var"|jb|Prim.println"
 end
const var"|h|TestPrim.op_Make" =
 begin
  var"|d|TestPrim.open"
  var"|d|Prim.op_Make"
 end
const var"|j|TestPrim.op_Element" =
 begin
  var"|d|TestPrim.open"
  var"|b|Prim.op_Element"
 end
const var"|l|TestPrim.make" =
 begin
  var"|d|TestPrim.open"
  var"|j|Prim.make"
 end
const var"|n|TestPrim.eq_i64" =
 begin
  var"|d|TestPrim.open"
  var"|fb|Prim.eq_i64"
 end
const var"|p|TestPrim.eq_f64" =
 begin
  var"|d|TestPrim.open"
  var"|hb|Prim.eq_f64"
 end
const var"|r|TestPrim.eq" =
 begin
  var"|d|TestPrim.open"
  var"|x|Prim.eq"
 end
const var"|t|TestPrim.__make_Eq" =
 begin
  var"|d|TestPrim.open"
  var"|r|Prim.__make_Eq"
 end
const var"|v|TestPrim.__get_equal" =
 begin
  var"|d|TestPrim.open"
  var"|p|Prim.__get_equal"
 end
const var"|x|TestPrim.Eq" =
 begin
  var"|d|TestPrim.open"
  var"|n|Prim.Eq"
 end
const var"|z|TestPrim.test" =
 (((var"|dc|Prim.println")(
    var"|d|TestPrim.open"))(
   (((((var"|rb|Prim.eq")(
        var"|d|TestPrim.open"))(
       var"|n|TestPrim.eq_i64"))(
      Int64(1) :: Int64))(
     Int64(1) :: Int64)):: Bool)):: Nothing
const var"|bb|TestPrim.Eq" =
 function (_::Any)
  var"|x|TestPrim.Eq"
 end
const var"|db|TestPrim.Prim" =
 function (_::Any)
  mlfs_Prim
 end
const var"|fb|TestPrim.__get_equal" =
 function (_::Any)
  var"|v|TestPrim.__get_equal"
 end
const var"|hb|TestPrim.__make_Eq" =
 function (_::Any)
  var"|t|TestPrim.__make_Eq"
 end
const var"|jb|TestPrim.eq" =
 function (_::Any)
  var"|r|TestPrim.eq"
 end
const var"|lb|TestPrim.eq_f64" =
 function (_::Any)
  var"|p|TestPrim.eq_f64"
 end
const var"|nb|TestPrim.eq_i64" =
 function (_::Any)
  var"|n|TestPrim.eq_i64"
 end
const var"|pb|TestPrim.make" =
 function (_::Any)
  var"|l|TestPrim.make"
 end
const var"|rb|TestPrim.op_Element" =
 function (_::Any)
  var"|j|TestPrim.op_Element"
 end
const var"|tb|TestPrim.op_Make" =
 function (_::Any)
  var"|h|TestPrim.op_Make"
 end
const var"|vb|TestPrim.println" =
 function (_::Any)
  var"|f|TestPrim.println"
 end
const var"|xb|TestPrim.test" =
 function (_::Any)
  var"|z|TestPrim.test"
 end
const var"|zb|TestPrim.|b|TestPrim.user_open" =
 function (_::Any)
  var"|d|TestPrim.open"
 end