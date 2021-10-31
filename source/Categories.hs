{-|
Module      : Categories
Description : Constrained categories
Copyright   : (c) Mario Román, 2020
License     : GPL-3
Maintainer  : mromang08@gmail.com
Stability   : experimental
Portability : POSIX

Provides a definition of category enriched over Hask, where the sets of objects
are represented by constraints.  Considers also functors and monoidal categories.
-}


module Categories where

-- | Definition of a category enriched over the language. The sets of objects
-- are represented by constraints.
class Category obj_c hom_c where
  unit :: (obj_c x) => hom_c x x                        
  comp :: (obj_c x) => hom_c y z -> hom_c x y -> hom_c x z


-- | Functors.
class (
  Category obj_c hom_c,
  Category obj_d hom_d,
  forall x . obj_c x => obj_d (f x)
) 
=> VFunctor objc c objd d f where

  map :: (obj_c x, obj_c y) => hom_c x y -> hom_d (f x) (f y)


-- | Bifunctors.
class (
  Category obj_c hom_c,
  Category obj_d hom_d,
  Category obj_e hom_e,
  forall x y. (obj_c x , obj_d y) => obj_e (f x y)
)
=> Bifunctor 
     obj_c hom_c 
     obj_d hom_d 
     obj_e hom_e f 
where

  bimap :: (
    obj_c x1,
    obj_c x2,
    obj_d y1,
    obj_d y2
  )
  => hom_c x1 x2 -> hom_d y1 y2 -> hom_e (f x1 y1) (f x2 y2)


-- | Profunctors.
class (
  Category obj_c hom_c,
  Category obj_d hom_d
)
=> Profunctor obj_c hom_c obj_d hom_d p
where

  dimap :: (
    objc x1,
    objc x2,
    objd y1,
    objd y2
   )
   => hom_c x2 x1 -> hom_d y1 y2 -> p x1 y1 -> p x2 y2


-- | Monoidal categories.
-- The definition follows that of a enriched monoidal category, taking Hask as the base of enrichment.
class (
  Category obj_a hom_a,
  Bifunctor
    obj_a hom_a
    obj_a hom_a 
    obj_a hom_a m,
  obj_a i
)
=> MonoidalCategory obj_a hom_a o i 
where
  
  alpha:: (
    obj_a x,
    obj_a y,
    obj_a z
  ) 
  => hom_a (M x (M y z)) (M (M x y) z)
  
  alphainv:: (
    obj_a x,
    obj_a y,
    obj_a z
  )
  => hom_a (M x (M y z)) (M (M x y) z)
  
  lambda    :: (obja x) => a (m x i) x

  lambdainv :: (obja x) => a x (m x i)

  rho       :: (obja x) => a (i `o` x) x

  rhoinv    :: (obja x) => a x (i `o` x)


-- | Monoidal actions as suitable bifunctors with the corresponding structure
-- maps.
class (
  MonoidalCategory obj_m hom_m m i,
  Bifunctor 
    obj_m hom_m
    obj_c hom_c
    obj_c hom_c f,
  Category objc c
)
=> MonoidalAction 
     obj_m hom_m o i
     obj_c hom_c f 
where
  
  unitor :: (obj_c x) => hom_c (f i x) x
  
  unitorinv :: (obj_c x) => hom_c x (f i x)
  
  multiplicator :: (
    obj_c x,
    obj_m p,
    obj_m q
  )
  => hom_c 
       (f p (f q x))
       (f (m p q) x) 
  
  multiplicatorinv :: (
    objc x
    objm p,
    objm q
   )
   => hom_c 
        (f (m p q) x)
        (f p (f q x)) 
