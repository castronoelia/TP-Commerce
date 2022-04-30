
type Producto = (String,Float)

nombreDeProducto :: Producto -> String
nombreDeProducto unProducto = fst unProducto

precioDeProducto :: Producto -> Float
precioDeProducto unProducto = snd unProducto

precioTotal :: Producto -> Float -> Float -> Float -> Float
precioTotal (nombre, precio) cantidad descuento costoDeEnvio =
  aplicarCostoDeEnvio (nombre, aplicarDescuento(nombre, precio) descuento * cantidad) costoDeEnvio



productoDeElite :: Producto -> Bool
productoDeElite unProducto = productoDeLujo unProducto && productoCodiciado unProducto && (not . productoCorriente) unProducto

aplicarDescuento :: Producto -> Float -> Float
aplicarDescuento unProducto unDescuento = precioDeProducto unProducto - (precioDeProducto  unProducto * unDescuento /100)

entregaSencilla :: String -> Bool
entregaSencilla unDia = even . length $ unDia

descodiciarProducto :: Producto -> String
descodiciarProducto unProducto = take 10 . nombreDeProducto $ unProducto

productoDeLujo :: Producto -> Bool
productoDeLujo unProducto = elem 'x' (nombreDeProducto unProducto) || elem 'z' (nombreDeProducto unProducto)
 


aplicarCostoDeEnvio :: Producto -> Float -> Float
aplicarCostoDeEnvio unProducto unCostoDeEnvio = precioDeProducto unProducto + unCostoDeEnvio

productoCodiciado :: Producto -> Bool
productoCodiciado unProducto = (> 10) . length $ nombreDeProducto unProducto

productoCorriente :: Producto-> Bool
productoCorriente unProducto = esVocal . head $ nombreDeProducto unProducto

esVocal :: Char -> Bool
esVocal unaLetra = elem unaLetra "aeiouAEIOU"

productoXL :: Producto -> String
productoXL unProducto = nombreDeProducto unProducto ++ " XL"

versionBarata :: Producto -> String
versionBarata unProducto = reverse . descodiciarProducto $   unProducto


{-
take :: Int -> String -> String
drop :: Int -> String -> String
head :: String -> Char
elem :: Char -> String -> Bool
reverse :: String -> String
-}
