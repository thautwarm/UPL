const mlfs_Prim =
 Int16(0)
const var"|b|Prim.op_Element" =
 u -> x -> x[u]
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
 (u -> x -> x[u])(
  UInt64(0) :: UInt64)
const var"|r|Prim.__make_Eq" =
 begin
  const var"|t|Prim.__make_Eq" =
   function (var"|v|Prim.equal" :: Function)
    (var"|v|Prim.equal")
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
  let @inline eq(x::Int64) = (y::Int64) -> x === y)
const var"|hb|Prim.eq_f64" =
 (((var"|d|Prim.op_Make")(
    var"|r|Prim.__make_Eq"))(
   "|l|Prim.Eq" :: String))(
  let @inline eq(x::Float64) = (y::Float64) -> x === y)
const var"|jb|Prim.test" =
 (((begin
     const var"|z|Prim.eq(0)" =
      var"|fb|Prim.eq_i64"
     function (var"|bb|Prim.lhs(1)" :: Any)
      function (var"|db|Prim.rhs(2)" :: Any)
       (((((var"|b|Prim.op_Element")(
            UInt64(0) :: UInt64))(
           var"|z|Prim.eq(0)"))(
          var"|bb|Prim.lhs(1)"))(
         var"|db|Prim.rhs(2)")):: Bool
      end
     end
    end)(
    Int64(1) :: Int64))(
   Int64(2) :: Int64)):: Bool
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
const var"|rb|Prim.a" =
 function (_::Any)
  Int16(0)
 end
const var"|tb|Prim.eq" =
 function (_::Any)
  var"|x|Prim.eq"
 end
const var"|vb|Prim.eq_f64" =
 function (_::Any)
  var"|hb|Prim.eq_f64"
 end
const var"|xb|Prim.eq_i64" =
 function (_::Any)
  var"|fb|Prim.eq_i64"
 end
const var"|zb|Prim.make" =
 function (_::Any)
  var"|j|Prim.make"
 end
const var"|bc|Prim.maker" =
 function (_::Any)
  Int16(0)
 end
const var"|dc|Prim.op_Element" =
 function (_::Any)
  var"|b|Prim.op_Element"
 end
const var"|fc|Prim.op_Make" =
 function (_::Any)
  var"|d|Prim.op_Make"
 end
const var"|hc|Prim.sub" =
 function (_::Any)
  Int16(0)
 end
const var"|jc|Prim.test" =
 function (_::Any)
  var"|jb|Prim.test"
 end