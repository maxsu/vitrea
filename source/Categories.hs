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
class Category ob_c hom_c where
  unit :: (ob_c x) => hom_c x x                        
  comp :: (ob_c x) => hom_c y z -> hom_c x y -> hom_c x z


-- | Functors.
class (
  Category ob_c hom_c,
  Category ob_d hom_d,
  forall x . ob_c x => ob_d (f x)
) 
=> VFunctor ob_c hom_c ob_d hom_d f where

  map :: (ob_c x, ob_c y) => hom_c x y -> hom_d (f x) (f y)


-- | Bifunctors.
class (
  Category ob_c hom_c,
  Category ob_d hom_d,
  Category ob_e hom_e,
  forall x y. 
    (ob_c x , ob_d y)
    => ob_e (f x y)
)
=> Bifunctor ob_c hom_c
             ob_d hom_d
             ob_e hom_e f 
where

  bimap :: (
    ob_c x1,
    ob_c x2,
    ob_d y1,
    ob_d y2
  )
  => hom_c x1 x2 
     -> hom_d y1 y2
     -> hom_e (f x1 y1) (f x2 y2)


-- | Profunctors.
class (
  Category ob_c hom_c,
  Category ob_d hom_d
)
=> Profunctor ob_c hom_c ob_d hom_d p
where

  dimap :: (
    ob_c x1,
    ob_c x2,
    ob_d y1,
    ob_d y2
   )
   => hom_c x2 x1
      -> hom_d y1 y2
      -> p x1 y1 
      -> p x2 y2


-- | Monoidal categories.
-- The definition follows that of a enriched monoidal category, taking Hask as the base of enrichment.
class (
  Category ob_a hom_a,
  Bifunctor
    ob_a hom_a
    ob_a hom_a 
    ob_a hom_a m,
  ob_a i
)
=> MonoidalCategory ob_a hom_a m i 
where
  
  alpha:: (
    ob_a x,
    ob_a y,
    ob_a z
  ) 
  => hom_a 
       (m x (m y z))
       (m (m x y) z)
  
  alphainv:: (
    ob_a x,
    ob_a y,
    ob_a z
  )
  => hom_a (m x (m y z))
           (m (m x y) z)
  
  lambda    :: (ob_a x) => hom_a (m x i) x
  lambdainv :: (ob_a x) => hom_a x (m x i)
  rho       :: (ob_a x) => hom_a (m i x) x
  rhoinv    :: (ob_a x) => hom_a x (m i x)


-- | Monoidal actions as suitable bifunctors with the corresponding structure
-- maps.
class (
  MonoidalCategory ob_m hom_m m i,
  Bifunctor ob_m hom_m
            ob_c hom_c
            ob_c hom_c f,
  Category ob_c hom_c
)
=> MonoidalAction ob_m hom_m o i
                  ob_c hom_c f 
where
  
  unitor :: (ob_c x) => hom_c (f i x) x
  unitorinv :: (ob_c x) => hom_c x (f i x)
  
  multiplicator :: (
    ob_c x,
    ob_m p,
    ob_m q
  )
  => hom_c 
       (f p (f q x))
       (f (m p q) x) 
  
  multiplicatorinv :: (
    ob_ x
    ob_m p,
    ob_m q
   )
   => hom_c 
        (f (m p q) x)
        (f p (f q x)) 
