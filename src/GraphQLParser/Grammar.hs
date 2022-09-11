{-# OPTIONS_GHC -w #-}
-- We have to disable -XStrictData here, as it doesn't play nicely with Happy.
{-# LANGUAGE NoStrictData #-}
module GraphQLParser.Grammar where

import Control.Monad.State (gets)
import Data.Coerce
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GraphQLParser.Error
import GraphQLParser.Monad
import GraphQLParser.Span
import GraphQLParser.Syntax
import GraphQLParser.Token
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn5 (Document)
	| HappyAbsSyn6 (Either ExecutableDefinition TypeSystemDefinitionOrExtension)
	| HappyAbsSyn7 (Definition)
	| HappyAbsSyn8 (TypeSystemDefinition)
	| HappyAbsSyn9 (SchemaDefinition)
	| HappyAbsSyn10 (RootOperationTypesDefinition)
	| HappyAbsSyn11 (NE.NonEmpty RootOperationTypeDefinition)
	| HappyAbsSyn12 (RootOperationTypeDefinition)
	| HappyAbsSyn13 (TypeDefinition)
	| HappyAbsSyn14 (ScalarTypeDefinition)
	| HappyAbsSyn15 (ObjectTypeDefinition)
	| HappyAbsSyn16 (InterfaceTypeDefinition)
	| HappyAbsSyn17 (Maybe ImplementsInterfaces)
	| HappyAbsSyn18 (Loc (NE.NonEmpty Name))
	| HappyAbsSyn19 (Maybe FieldsDefinition)
	| HappyAbsSyn20 (NE.NonEmpty FieldDefinition)
	| HappyAbsSyn21 (FieldDefinition)
	| HappyAbsSyn22 (UnionTypeDefinition)
	| HappyAbsSyn23 (Loc [Name])
	| HappyAbsSyn24 (EnumTypeDefinition)
	| HappyAbsSyn25 ([EnumValueDefinition])
	| HappyAbsSyn26 (EnumValueDefinition)
	| HappyAbsSyn27 (InputObjectTypeDefinition)
	| HappyAbsSyn28 (DirectiveDefinition)
	| HappyAbsSyn29 (Loc [DirectiveLocation])
	| HappyAbsSyn30 (DirectiveLocation)
	| HappyAbsSyn31 (Loc ExecutableDirectiveLocation)
	| HappyAbsSyn32 (Loc TypeSystemDirectiveLocation)
	| HappyAbsSyn33 (Maybe InputFieldsDefinition)
	| HappyAbsSyn34 (Maybe ArgumentsDefinition)
	| HappyAbsSyn35 (NE.NonEmpty InputValueDefinition)
	| HappyAbsSyn36 (InputValueDefinition)
	| HappyAbsSyn37 (ExecutableDefinition)
	| HappyAbsSyn38 (OperationDefinition)
	| HappyAbsSyn39 (FragmentDefinition)
	| HappyAbsSyn40 (FragmentSpread)
	| HappyAbsSyn41 (InlineFragment)
	| HappyAbsSyn42 (Field)
	| HappyAbsSyn43 (Loc SelectionSet)
	| HappyAbsSyn44 ([Selection])
	| HappyAbsSyn45 (Selection)
	| HappyAbsSyn46 (Maybe Value)
	| HappyAbsSyn47 ([Value])
	| HappyAbsSyn48 (Value)
	| HappyAbsSyn50 (Loc Text)
	| HappyAbsSyn54 (HashMap Name Value)
	| HappyAbsSyn55 ((Name, Value))
	| HappyAbsSyn56 (Loc OperationType)
	| HappyAbsSyn57 (Loc (Maybe Name, Name))
	| HappyAbsSyn58 (Loc Name)
	| HappyAbsSyn59 (Arguments)
	| HappyAbsSyn60 (Loc (HashMap Name Value))
	| HappyAbsSyn61 (Loc (Name, Value))
	| HappyAbsSyn62 (VariablesDefinition)
	| HappyAbsSyn63 (NE.NonEmpty VariableDefinition)
	| HappyAbsSyn64 (VariableDefinition)
	| HappyAbsSyn65 (TypeCondition)
	| HappyAbsSyn66 (Type)
	| HappyAbsSyn67 (Maybe (Loc Directives))
	| HappyAbsSyn68 (Loc (NE.NonEmpty Directive))
	| HappyAbsSyn69 (Directive)
	| HappyAbsSyn70 (Maybe (Loc Description))
	| HappyAbsSyn71 (Maybe Span)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233,
 action_234,
 action_235,
 action_236,
 action_237,
 action_238,
 action_239,
 action_240,
 action_241,
 action_242,
 action_243,
 action_244,
 action_245,
 action_246,
 action_247,
 action_248,
 action_249,
 action_250,
 action_251,
 action_252,
 action_253 :: () => Prelude.Int -> ({-HappyReduction (Parser) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Parser) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Parser) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Parser) HappyAbsSyn)

happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118,
 happyReduce_119,
 happyReduce_120,
 happyReduce_121,
 happyReduce_122,
 happyReduce_123,
 happyReduce_124,
 happyReduce_125,
 happyReduce_126,
 happyReduce_127,
 happyReduce_128,
 happyReduce_129,
 happyReduce_130,
 happyReduce_131,
 happyReduce_132,
 happyReduce_133,
 happyReduce_134,
 happyReduce_135,
 happyReduce_136,
 happyReduce_137,
 happyReduce_138,
 happyReduce_139,
 happyReduce_140,
 happyReduce_141,
 happyReduce_142,
 happyReduce_143,
 happyReduce_144,
 happyReduce_145,
 happyReduce_146,
 happyReduce_147,
 happyReduce_148,
 happyReduce_149 :: () => ({-HappyReduction (Parser) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Parser) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Parser) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Parser) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,572) ([0,0,0,0,25088,16,16384,516,0,0,0,0,0,0,0,0,2,0,0,0,8192,262,0,8260,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4097,32,0,0,0,32768,27673,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,4096,8,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6272,4,4096,129,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,2064,0,0,0,0,0,0,256,16,0,0,0,0,0,0,32768,0,0,0,0,0,0,4,4096,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,2,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,128,0,0,0,0,0,0,16,1,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,2,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,64,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,16,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,512,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,32776,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,4,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,4,32,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,1536,1,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32784,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,57670,1,0,0,0,0,0,0,1024,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,512,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,32768,65,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,2049,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,32784,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,32768,57668,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,8,0,0,0,0,32,0,36114,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,4608,1925,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,2049,0,0,0,0,0,0,0,0,8,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,1024,32,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2049,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,8224,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,4096,128,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,65535,515,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,8,32768,57668,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,18432,7700,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,65534,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,65534,15,0,0,0,0,0,2048,0,17920,481,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,8,0,0,0,0,0,0,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseGraphQLDocument","%start_parseName","graphqlDocument","executableOrTypeSystemDefiniton","graphqlDefinition","typeSystemDefinition","schemaDefinition","rootOperationTypesDefinition","rootOperationTypesDefinition_","rootOperationTypeDefinition","typeDefinition","scalarTypeDefinition","objectTypeDefinition","interfaceTypeDefinition","implementsInterfaces","implementsInterfaces_","fieldsDefinition","fieldDefinitions","fieldDefinition","unionTypeDefinition","unionMembers","enumTypeDefinition","enumValuesDefinition","enumValueDefinition","inputObjectTypeDefinition","directiveDefinition","directiveLocations","directiveLocation","executableDirectiveLocation","typeSystemDirectiveLocation","inputFieldsDefinition","argumentsDefinition","inputValuesDefinition","inputValueDefinition","executableDefinition","typedOperationDefinition","fragmentDefinition","fragmentSpread","inlineFragment","field","selectionSet","selections","selection","optValue","values","value","valueConst","variable","stringValue","vlist","vobject","object","objectField","operationType","aliasAndName","name","arguments","arguments_","argument","variableDefinitions","variableDefinitions_","variableDefinition","typeCondition","type","directives","directives_","directive","description","optRepeatable","'directive'","'enum'","'fragment'","'implements'","'interface'","'input'","'query'","'mutation'","'null'","'on'","'repeatable'","'scalar'","'schema'","'subscription'","'type'","'union'","'QUERY'","'MUTATION'","'SUBSCRIPTION'","'FIELD'","'FRAGMENT_DEFINITION'","'FRAGMENT_SPREAD'","'INLINE_FRAGMENT'","'VARIABLE_DEFINITION'","'SCHEMA'","'SCALAR'","'OBJECT'","'FIELD_DEFINITION'","'ARGUMENT_DEFINITION'","'INTERFACE'","'UNION'","'ENUM'","'ENUM_VALUE'","'INPUT_OBJECT'","'INPUT_FIELD_DEFINITION'","'@'","'$'","'&'","'!'","'\"'","':'","'='","'|'","'{'","'}'","'['","']'","'('","')'","'...'","'\"\"\"'","int","float","bool","blockString","string","ident","dir","%eof"]
        bit_start = st Prelude.* 130
        bit_end = (st Prelude.+ 1) Prelude.* 130
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..129]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (74) = happyShift action_21
action_0 (78) = happyShift action_22
action_0 (79) = happyShift action_23
action_0 (85) = happyShift action_24
action_0 (111) = happyShift action_25
action_0 (115) = happyShift action_26
action_0 (122) = happyShift action_27
action_0 (5) = happyGoto action_30
action_0 (7) = happyGoto action_31
action_0 (8) = happyGoto action_4
action_0 (9) = happyGoto action_5
action_0 (13) = happyGoto action_6
action_0 (14) = happyGoto action_7
action_0 (15) = happyGoto action_8
action_0 (16) = happyGoto action_9
action_0 (22) = happyGoto action_10
action_0 (24) = happyGoto action_11
action_0 (27) = happyGoto action_12
action_0 (28) = happyGoto action_13
action_0 (37) = happyGoto action_14
action_0 (38) = happyGoto action_15
action_0 (39) = happyGoto action_16
action_0 (43) = happyGoto action_17
action_0 (51) = happyGoto action_18
action_0 (56) = happyGoto action_19
action_0 (70) = happyGoto action_20
action_0 _ = happyReduce_147

action_1 (128) = happyShift action_29
action_1 (58) = happyGoto action_28
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (74) = happyShift action_21
action_2 (78) = happyShift action_22
action_2 (79) = happyShift action_23
action_2 (85) = happyShift action_24
action_2 (111) = happyShift action_25
action_2 (115) = happyShift action_26
action_2 (122) = happyShift action_27
action_2 (7) = happyGoto action_3
action_2 (8) = happyGoto action_4
action_2 (9) = happyGoto action_5
action_2 (13) = happyGoto action_6
action_2 (14) = happyGoto action_7
action_2 (15) = happyGoto action_8
action_2 (16) = happyGoto action_9
action_2 (22) = happyGoto action_10
action_2 (24) = happyGoto action_11
action_2 (27) = happyGoto action_12
action_2 (28) = happyGoto action_13
action_2 (37) = happyGoto action_14
action_2 (38) = happyGoto action_15
action_2 (39) = happyGoto action_16
action_2 (43) = happyGoto action_17
action_2 (51) = happyGoto action_18
action_2 (56) = happyGoto action_19
action_2 (70) = happyGoto action_20
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_6

action_5 _ = happyReduce_8

action_6 _ = happyReduce_9

action_7 _ = happyReduce_16

action_8 _ = happyReduce_17

action_9 _ = happyReduce_18

action_10 _ = happyReduce_19

action_11 _ = happyReduce_20

action_12 _ = happyReduce_21

action_13 _ = happyReduce_10

action_14 _ = happyReduce_7

action_15 _ = happyReduce_76

action_16 _ = happyReduce_78

action_17 _ = happyReduce_77

action_18 _ = happyReduce_146

action_19 (107) = happyShift action_58
action_19 (119) = happyShift action_59
action_19 (128) = happyShift action_29
action_19 (58) = happyGoto action_53
action_19 (62) = happyGoto action_54
action_19 (67) = happyGoto action_55
action_19 (68) = happyGoto action_56
action_19 (69) = happyGoto action_57
action_19 _ = happyReduce_141

action_20 (72) = happyShift action_45
action_20 (73) = happyShift action_46
action_20 (76) = happyShift action_47
action_20 (77) = happyShift action_48
action_20 (83) = happyShift action_49
action_20 (84) = happyShift action_50
action_20 (86) = happyShift action_51
action_20 (87) = happyShift action_52
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (128) = happyShift action_29
action_21 (58) = happyGoto action_44
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_121

action_23 _ = happyReduce_122

action_24 _ = happyReduce_123

action_25 (111) = happyShift action_42
action_25 (127) = happyShift action_43
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (121) = happyShift action_41
action_26 (128) = happyShift action_29
action_26 (40) = happyGoto action_34
action_26 (41) = happyGoto action_35
action_26 (42) = happyGoto action_36
action_26 (44) = happyGoto action_37
action_26 (45) = happyGoto action_38
action_26 (57) = happyGoto action_39
action_26 (58) = happyGoto action_40
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (126) = happyShift action_33
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (130) = happyAccept
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_126

action_30 (130) = happyAccept
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (74) = happyShift action_21
action_31 (78) = happyShift action_22
action_31 (79) = happyShift action_23
action_31 (85) = happyShift action_24
action_31 (111) = happyShift action_25
action_31 (115) = happyShift action_26
action_31 (122) = happyShift action_27
action_31 (130) = happyReduce_2
action_31 (5) = happyGoto action_32
action_31 (7) = happyGoto action_31
action_31 (8) = happyGoto action_4
action_31 (9) = happyGoto action_5
action_31 (13) = happyGoto action_6
action_31 (14) = happyGoto action_7
action_31 (15) = happyGoto action_8
action_31 (16) = happyGoto action_9
action_31 (22) = happyGoto action_10
action_31 (24) = happyGoto action_11
action_31 (27) = happyGoto action_12
action_31 (28) = happyGoto action_13
action_31 (37) = happyGoto action_14
action_31 (38) = happyGoto action_15
action_31 (39) = happyGoto action_16
action_31 (43) = happyGoto action_17
action_31 (51) = happyGoto action_18
action_31 (56) = happyGoto action_19
action_31 (70) = happyGoto action_20
action_31 _ = happyReduce_147

action_32 _ = happyReduce_3

action_33 (122) = happyShift action_90
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_95

action_35 _ = happyReduce_96

action_36 _ = happyReduce_94

action_37 (116) = happyShift action_89
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (121) = happyShift action_41
action_38 (128) = happyShift action_29
action_38 (40) = happyGoto action_34
action_38 (41) = happyGoto action_35
action_38 (42) = happyGoto action_36
action_38 (44) = happyGoto action_88
action_38 (45) = happyGoto action_38
action_38 (57) = happyGoto action_39
action_38 (58) = happyGoto action_40
action_38 _ = happyReduce_92

action_39 (107) = happyShift action_58
action_39 (119) = happyShift action_87
action_39 (59) = happyGoto action_85
action_39 (67) = happyGoto action_86
action_39 (68) = happyGoto action_56
action_39 (69) = happyGoto action_57
action_39 _ = happyReduce_141

action_40 (112) = happyShift action_84
action_40 _ = happyReduce_125

action_41 (81) = happyShift action_79
action_41 (107) = happyShift action_58
action_41 (128) = happyShift action_29
action_41 (58) = happyGoto action_81
action_41 (65) = happyGoto action_82
action_41 (67) = happyGoto action_83
action_41 (68) = happyGoto action_56
action_41 (69) = happyGoto action_57
action_41 _ = happyReduce_141

action_42 _ = happyReduce_111

action_43 (111) = happyShift action_80
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (81) = happyShift action_79
action_44 (65) = happyGoto action_78
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (107) = happyShift action_77
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (128) = happyShift action_29
action_46 (58) = happyGoto action_76
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (128) = happyShift action_29
action_47 (58) = happyGoto action_75
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (128) = happyShift action_29
action_48 (58) = happyGoto action_74
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (128) = happyShift action_29
action_49 (58) = happyGoto action_73
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (107) = happyShift action_58
action_50 (67) = happyGoto action_72
action_50 (68) = happyGoto action_56
action_50 (69) = happyGoto action_57
action_50 _ = happyReduce_141

action_51 (128) = happyShift action_29
action_51 (58) = happyGoto action_71
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (128) = happyShift action_29
action_52 (58) = happyGoto action_70
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (107) = happyShift action_58
action_53 (119) = happyShift action_59
action_53 (62) = happyGoto action_68
action_53 (67) = happyGoto action_69
action_53 (68) = happyGoto action_56
action_53 (69) = happyGoto action_57
action_53 _ = happyReduce_141

action_54 (107) = happyShift action_58
action_54 (67) = happyGoto action_67
action_54 (68) = happyGoto action_56
action_54 (69) = happyGoto action_57
action_54 _ = happyReduce_141

action_55 (115) = happyShift action_26
action_55 (43) = happyGoto action_66
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_140

action_57 (107) = happyShift action_58
action_57 (68) = happyGoto action_65
action_57 (69) = happyGoto action_57
action_57 _ = happyReduce_142

action_58 (129) = happyShift action_64
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (108) = happyShift action_63
action_59 (50) = happyGoto action_60
action_59 (63) = happyGoto action_61
action_59 (64) = happyGoto action_62
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (112) = happyShift action_120
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (120) = happyShift action_119
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (108) = happyShift action_63
action_62 (50) = happyGoto action_60
action_62 (63) = happyGoto action_118
action_62 (64) = happyGoto action_62
action_62 _ = happyReduce_133

action_63 (128) = happyShift action_117
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (119) = happyShift action_87
action_64 (59) = happyGoto action_116
action_64 _ = happyReduce_144

action_65 _ = happyReduce_143

action_66 _ = happyReduce_79

action_67 (115) = happyShift action_26
action_67 (43) = happyGoto action_115
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (107) = happyShift action_58
action_68 (67) = happyGoto action_114
action_68 (68) = happyGoto action_56
action_68 (69) = happyGoto action_57
action_68 _ = happyReduce_141

action_69 (115) = happyShift action_26
action_69 (43) = happyGoto action_113
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (107) = happyShift action_58
action_70 (67) = happyGoto action_112
action_70 (68) = happyGoto action_56
action_70 (69) = happyGoto action_57
action_70 _ = happyReduce_141

action_71 (75) = happyShift action_107
action_71 (17) = happyGoto action_111
action_71 (18) = happyGoto action_106
action_71 _ = happyReduce_26

action_72 (115) = happyShift action_110
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (107) = happyShift action_58
action_73 (67) = happyGoto action_109
action_73 (68) = happyGoto action_56
action_73 (69) = happyGoto action_57
action_73 _ = happyReduce_141

action_74 (107) = happyShift action_58
action_74 (67) = happyGoto action_108
action_74 (68) = happyGoto action_56
action_74 (69) = happyGoto action_57
action_74 _ = happyReduce_141

action_75 (75) = happyShift action_107
action_75 (17) = happyGoto action_105
action_75 (18) = happyGoto action_106
action_75 _ = happyReduce_26

action_76 (107) = happyShift action_58
action_76 (67) = happyGoto action_104
action_76 (68) = happyGoto action_56
action_76 (69) = happyGoto action_57
action_76 _ = happyReduce_141

action_77 (129) = happyShift action_103
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (107) = happyShift action_58
action_78 (67) = happyGoto action_102
action_78 (68) = happyGoto action_56
action_78 (69) = happyGoto action_57
action_78 _ = happyReduce_141

action_79 (128) = happyShift action_29
action_79 (58) = happyGoto action_101
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_112

action_81 (107) = happyShift action_58
action_81 (67) = happyGoto action_100
action_81 (68) = happyGoto action_56
action_81 (69) = happyGoto action_57
action_81 _ = happyReduce_141

action_82 (107) = happyShift action_58
action_82 (67) = happyGoto action_99
action_82 (68) = happyGoto action_56
action_82 (69) = happyGoto action_57
action_82 _ = happyReduce_141

action_83 (115) = happyShift action_26
action_83 (43) = happyGoto action_98
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (128) = happyShift action_29
action_84 (58) = happyGoto action_97
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (107) = happyShift action_58
action_85 (67) = happyGoto action_96
action_85 (68) = happyGoto action_56
action_85 (69) = happyGoto action_57
action_85 _ = happyReduce_141

action_86 (115) = happyShift action_26
action_86 (43) = happyGoto action_95
action_86 _ = happyReduce_87

action_87 (108) = happyShift action_94
action_87 (128) = happyShift action_29
action_87 (58) = happyGoto action_91
action_87 (60) = happyGoto action_92
action_87 (61) = happyGoto action_93
action_87 _ = happyFail (happyExpListPerState 87)

action_88 _ = happyReduce_93

action_89 _ = happyReduce_91

action_90 _ = happyReduce_113

action_91 (112) = happyShift action_146
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (108) = happyShift action_94
action_92 (120) = happyShift action_145
action_92 (128) = happyShift action_29
action_92 (58) = happyGoto action_91
action_92 (61) = happyGoto action_144
action_92 _ = happyFail (happyExpListPerState 92)

action_93 _ = happyReduce_128

action_94 (128) = happyShift action_29
action_94 (58) = happyGoto action_143
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_90

action_96 (115) = happyShift action_26
action_96 (43) = happyGoto action_142
action_96 _ = happyReduce_88

action_97 _ = happyReduce_124

action_98 _ = happyReduce_86

action_99 (115) = happyShift action_26
action_99 (43) = happyGoto action_141
action_99 _ = happyFail (happyExpListPerState 99)

action_100 _ = happyReduce_84

action_101 _ = happyReduce_136

action_102 (115) = happyShift action_26
action_102 (43) = happyGoto action_140
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (119) = happyShift action_139
action_103 (34) = happyGoto action_138
action_103 _ = happyReduce_72

action_104 (115) = happyShift action_137
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (107) = happyShift action_58
action_105 (67) = happyGoto action_136
action_105 (68) = happyGoto action_56
action_105 (69) = happyGoto action_57
action_105 _ = happyReduce_141

action_106 (109) = happyShift action_135
action_106 _ = happyReduce_25

action_107 (109) = happyShift action_134
action_107 (128) = happyShift action_29
action_107 (58) = happyGoto action_133
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (115) = happyShift action_132
action_108 (33) = happyGoto action_131
action_108 _ = happyReduce_70

action_109 _ = happyReduce_22

action_110 (78) = happyShift action_22
action_110 (79) = happyShift action_23
action_110 (85) = happyShift action_24
action_110 (10) = happyGoto action_127
action_110 (11) = happyGoto action_128
action_110 (12) = happyGoto action_129
action_110 (56) = happyGoto action_130
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (107) = happyShift action_58
action_111 (67) = happyGoto action_126
action_111 (68) = happyGoto action_56
action_111 (69) = happyGoto action_57
action_111 _ = happyReduce_141

action_112 (113) = happyShift action_125
action_112 _ = happyFail (happyExpListPerState 112)

action_113 _ = happyReduce_80

action_114 (115) = happyShift action_26
action_114 (43) = happyGoto action_124
action_114 _ = happyFail (happyExpListPerState 114)

action_115 _ = happyReduce_81

action_116 _ = happyReduce_145

action_117 _ = happyReduce_110

action_118 _ = happyReduce_134

action_119 _ = happyReduce_132

action_120 (117) = happyShift action_123
action_120 (128) = happyShift action_29
action_120 (58) = happyGoto action_121
action_120 (66) = happyGoto action_122
action_120 _ = happyFail (happyExpListPerState 120)

action_121 _ = happyReduce_137

action_122 (80) = happyShift action_153
action_122 (110) = happyShift action_183
action_122 (111) = happyShift action_25
action_122 (115) = happyShift action_154
action_122 (117) = happyShift action_155
action_122 (122) = happyShift action_27
action_122 (123) = happyShift action_156
action_122 (124) = happyShift action_157
action_122 (125) = happyShift action_158
action_122 (46) = happyGoto action_181
action_122 (49) = happyGoto action_182
action_122 (51) = happyGoto action_150
action_122 (52) = happyGoto action_151
action_122 (53) = happyGoto action_152
action_122 _ = happyReduce_98

action_123 (117) = happyShift action_123
action_123 (128) = happyShift action_29
action_123 (58) = happyGoto action_121
action_123 (66) = happyGoto action_180
action_123 _ = happyFail (happyExpListPerState 123)

action_124 _ = happyReduce_82

action_125 (114) = happyShift action_179
action_125 (128) = happyShift action_29
action_125 (23) = happyGoto action_177
action_125 (58) = happyGoto action_178
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (115) = happyShift action_169
action_126 (19) = happyGoto action_176
action_126 _ = happyReduce_31

action_127 (116) = happyShift action_175
action_127 _ = happyFail (happyExpListPerState 127)

action_128 _ = happyReduce_12

action_129 (78) = happyShift action_22
action_129 (79) = happyShift action_23
action_129 (85) = happyShift action_24
action_129 (11) = happyGoto action_174
action_129 (12) = happyGoto action_129
action_129 (56) = happyGoto action_130
action_129 _ = happyReduce_13

action_130 (112) = happyShift action_173
action_130 _ = happyFail (happyExpListPerState 130)

action_131 _ = happyReduce_43

action_132 (111) = happyShift action_25
action_132 (122) = happyShift action_27
action_132 (35) = happyGoto action_172
action_132 (36) = happyGoto action_161
action_132 (51) = happyGoto action_18
action_132 (70) = happyGoto action_162
action_132 _ = happyReduce_147

action_133 _ = happyReduce_29

action_134 (128) = happyShift action_29
action_134 (58) = happyGoto action_171
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (128) = happyShift action_29
action_135 (58) = happyGoto action_170
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (115) = happyShift action_169
action_136 (19) = happyGoto action_168
action_136 _ = happyReduce_31

action_137 (111) = happyShift action_25
action_137 (122) = happyShift action_27
action_137 (25) = happyGoto action_165
action_137 (26) = happyGoto action_166
action_137 (51) = happyGoto action_18
action_137 (70) = happyGoto action_167
action_137 _ = happyReduce_147

action_138 (82) = happyShift action_164
action_138 (71) = happyGoto action_163
action_138 _ = happyReduce_149

action_139 (111) = happyShift action_25
action_139 (122) = happyShift action_27
action_139 (35) = happyGoto action_160
action_139 (36) = happyGoto action_161
action_139 (51) = happyGoto action_18
action_139 (70) = happyGoto action_162
action_139 _ = happyReduce_147

action_140 _ = happyReduce_83

action_141 _ = happyReduce_85

action_142 _ = happyReduce_89

action_143 (112) = happyShift action_159
action_143 _ = happyFail (happyExpListPerState 143)

action_144 _ = happyReduce_129

action_145 _ = happyReduce_127

action_146 (80) = happyShift action_153
action_146 (108) = happyShift action_63
action_146 (111) = happyShift action_25
action_146 (115) = happyShift action_154
action_146 (117) = happyShift action_155
action_146 (122) = happyShift action_27
action_146 (123) = happyShift action_156
action_146 (124) = happyShift action_157
action_146 (125) = happyShift action_158
action_146 (48) = happyGoto action_147
action_146 (49) = happyGoto action_148
action_146 (50) = happyGoto action_149
action_146 (51) = happyGoto action_150
action_146 (52) = happyGoto action_151
action_146 (53) = happyGoto action_152
action_146 _ = happyFail (happyExpListPerState 146)

action_147 _ = happyReduce_130

action_148 _ = happyReduce_101

action_149 _ = happyReduce_102

action_150 _ = happyReduce_104

action_151 _ = happyReduce_108

action_152 _ = happyReduce_109

action_153 _ = happyReduce_103

action_154 (116) = happyShift action_207
action_154 (128) = happyShift action_29
action_154 (54) = happyGoto action_204
action_154 (55) = happyGoto action_205
action_154 (58) = happyGoto action_206
action_154 _ = happyFail (happyExpListPerState 154)

action_155 (80) = happyShift action_153
action_155 (108) = happyShift action_63
action_155 (111) = happyShift action_25
action_155 (115) = happyShift action_154
action_155 (117) = happyShift action_155
action_155 (118) = happyShift action_203
action_155 (122) = happyShift action_27
action_155 (123) = happyShift action_156
action_155 (124) = happyShift action_157
action_155 (125) = happyShift action_158
action_155 (47) = happyGoto action_201
action_155 (48) = happyGoto action_202
action_155 (49) = happyGoto action_148
action_155 (50) = happyGoto action_149
action_155 (51) = happyGoto action_150
action_155 (52) = happyGoto action_151
action_155 (53) = happyGoto action_152
action_155 _ = happyFail (happyExpListPerState 155)

action_156 _ = happyReduce_106

action_157 _ = happyReduce_105

action_158 _ = happyReduce_107

action_159 (80) = happyShift action_153
action_159 (108) = happyShift action_63
action_159 (111) = happyShift action_25
action_159 (115) = happyShift action_154
action_159 (117) = happyShift action_155
action_159 (122) = happyShift action_27
action_159 (123) = happyShift action_156
action_159 (124) = happyShift action_157
action_159 (125) = happyShift action_158
action_159 (48) = happyGoto action_200
action_159 (49) = happyGoto action_148
action_159 (50) = happyGoto action_149
action_159 (51) = happyGoto action_150
action_159 (52) = happyGoto action_151
action_159 (53) = happyGoto action_152
action_159 _ = happyFail (happyExpListPerState 159)

action_160 (120) = happyShift action_199
action_160 _ = happyFail (happyExpListPerState 160)

action_161 (111) = happyShift action_25
action_161 (122) = happyShift action_27
action_161 (128) = happyReduce_147
action_161 (35) = happyGoto action_198
action_161 (36) = happyGoto action_161
action_161 (51) = happyGoto action_18
action_161 (70) = happyGoto action_162
action_161 _ = happyReduce_73

action_162 (128) = happyShift action_29
action_162 (58) = happyGoto action_197
action_162 _ = happyFail (happyExpListPerState 162)

action_163 (81) = happyShift action_196
action_163 _ = happyFail (happyExpListPerState 163)

action_164 _ = happyReduce_148

action_165 (116) = happyShift action_195
action_165 _ = happyFail (happyExpListPerState 165)

action_166 (111) = happyShift action_25
action_166 (122) = happyShift action_27
action_166 (128) = happyReduce_147
action_166 (25) = happyGoto action_194
action_166 (26) = happyGoto action_166
action_166 (51) = happyGoto action_18
action_166 (70) = happyGoto action_167
action_166 _ = happyReduce_40

action_167 (128) = happyShift action_29
action_167 (58) = happyGoto action_193
action_167 _ = happyFail (happyExpListPerState 167)

action_168 _ = happyReduce_24

action_169 (111) = happyShift action_25
action_169 (122) = happyShift action_27
action_169 (20) = happyGoto action_190
action_169 (21) = happyGoto action_191
action_169 (51) = happyGoto action_18
action_169 (70) = happyGoto action_192
action_169 _ = happyReduce_147

action_170 _ = happyReduce_27

action_171 _ = happyReduce_28

action_172 (116) = happyShift action_189
action_172 _ = happyFail (happyExpListPerState 172)

action_173 (128) = happyShift action_29
action_173 (58) = happyGoto action_188
action_173 _ = happyFail (happyExpListPerState 173)

action_174 _ = happyReduce_14

action_175 _ = happyReduce_11

action_176 _ = happyReduce_23

action_177 _ = happyReduce_35

action_178 (114) = happyShift action_187
action_178 _ = happyReduce_37

action_179 (128) = happyShift action_29
action_179 (23) = happyGoto action_186
action_179 (58) = happyGoto action_178
action_179 _ = happyFail (happyExpListPerState 179)

action_180 (110) = happyShift action_183
action_180 (118) = happyShift action_185
action_180 _ = happyFail (happyExpListPerState 180)

action_181 (107) = happyShift action_58
action_181 (67) = happyGoto action_184
action_181 (68) = happyGoto action_56
action_181 (69) = happyGoto action_57
action_181 _ = happyReduce_141

action_182 _ = happyReduce_97

action_183 _ = happyReduce_138

action_184 _ = happyReduce_135

action_185 _ = happyReduce_139

action_186 _ = happyReduce_36

action_187 (128) = happyShift action_29
action_187 (23) = happyGoto action_242
action_187 (58) = happyGoto action_178
action_187 _ = happyFail (happyExpListPerState 187)

action_188 _ = happyReduce_15

action_189 _ = happyReduce_69

action_190 (116) = happyShift action_241
action_190 _ = happyFail (happyExpListPerState 190)

action_191 (111) = happyShift action_25
action_191 (122) = happyShift action_27
action_191 (128) = happyReduce_147
action_191 (20) = happyGoto action_240
action_191 (21) = happyGoto action_191
action_191 (51) = happyGoto action_18
action_191 (70) = happyGoto action_192
action_191 _ = happyReduce_32

action_192 (128) = happyShift action_29
action_192 (58) = happyGoto action_239
action_192 _ = happyFail (happyExpListPerState 192)

action_193 (107) = happyShift action_58
action_193 (67) = happyGoto action_238
action_193 (68) = happyGoto action_56
action_193 (69) = happyGoto action_57
action_193 _ = happyReduce_141

action_194 _ = happyReduce_41

action_195 _ = happyReduce_39

action_196 (88) = happyShift action_218
action_196 (89) = happyShift action_219
action_196 (90) = happyShift action_220
action_196 (91) = happyShift action_221
action_196 (92) = happyShift action_222
action_196 (93) = happyShift action_223
action_196 (94) = happyShift action_224
action_196 (95) = happyShift action_225
action_196 (96) = happyShift action_226
action_196 (97) = happyShift action_227
action_196 (98) = happyShift action_228
action_196 (99) = happyShift action_229
action_196 (100) = happyShift action_230
action_196 (101) = happyShift action_231
action_196 (102) = happyShift action_232
action_196 (103) = happyShift action_233
action_196 (104) = happyShift action_234
action_196 (105) = happyShift action_235
action_196 (106) = happyShift action_236
action_196 (114) = happyShift action_237
action_196 (29) = happyGoto action_214
action_196 (30) = happyGoto action_215
action_196 (31) = happyGoto action_216
action_196 (32) = happyGoto action_217
action_196 _ = happyFail (happyExpListPerState 196)

action_197 (112) = happyShift action_213
action_197 _ = happyFail (happyExpListPerState 197)

action_198 _ = happyReduce_74

action_199 _ = happyReduce_71

action_200 _ = happyReduce_131

action_201 (118) = happyShift action_212
action_201 _ = happyFail (happyExpListPerState 201)

action_202 (80) = happyShift action_153
action_202 (108) = happyShift action_63
action_202 (111) = happyShift action_25
action_202 (115) = happyShift action_154
action_202 (117) = happyShift action_155
action_202 (122) = happyShift action_27
action_202 (123) = happyShift action_156
action_202 (124) = happyShift action_157
action_202 (125) = happyShift action_158
action_202 (47) = happyGoto action_211
action_202 (48) = happyGoto action_202
action_202 (49) = happyGoto action_148
action_202 (50) = happyGoto action_149
action_202 (51) = happyGoto action_150
action_202 (52) = happyGoto action_151
action_202 (53) = happyGoto action_152
action_202 _ = happyReduce_99

action_203 _ = happyReduce_114

action_204 (116) = happyShift action_210
action_204 _ = happyFail (happyExpListPerState 204)

action_205 (128) = happyShift action_29
action_205 (54) = happyGoto action_209
action_205 (55) = happyGoto action_205
action_205 (58) = happyGoto action_206
action_205 _ = happyReduce_118

action_206 (112) = happyShift action_208
action_206 _ = happyFail (happyExpListPerState 206)

action_207 _ = happyReduce_116

action_208 (80) = happyShift action_153
action_208 (108) = happyShift action_63
action_208 (111) = happyShift action_25
action_208 (115) = happyShift action_154
action_208 (117) = happyShift action_155
action_208 (122) = happyShift action_27
action_208 (123) = happyShift action_156
action_208 (124) = happyShift action_157
action_208 (125) = happyShift action_158
action_208 (48) = happyGoto action_247
action_208 (49) = happyGoto action_148
action_208 (50) = happyGoto action_149
action_208 (51) = happyGoto action_150
action_208 (52) = happyGoto action_151
action_208 (53) = happyGoto action_152
action_208 _ = happyFail (happyExpListPerState 208)

action_209 _ = happyReduce_119

action_210 _ = happyReduce_117

action_211 _ = happyReduce_100

action_212 _ = happyReduce_115

action_213 (117) = happyShift action_123
action_213 (128) = happyShift action_29
action_213 (58) = happyGoto action_121
action_213 (66) = happyGoto action_246
action_213 _ = happyFail (happyExpListPerState 213)

action_214 _ = happyReduce_44

action_215 (114) = happyShift action_245
action_215 _ = happyReduce_46

action_216 _ = happyReduce_48

action_217 _ = happyReduce_49

action_218 _ = happyReduce_50

action_219 _ = happyReduce_51

action_220 _ = happyReduce_52

action_221 _ = happyReduce_53

action_222 _ = happyReduce_54

action_223 _ = happyReduce_55

action_224 _ = happyReduce_56

action_225 _ = happyReduce_57

action_226 _ = happyReduce_58

action_227 _ = happyReduce_59

action_228 _ = happyReduce_60

action_229 _ = happyReduce_61

action_230 _ = happyReduce_62

action_231 _ = happyReduce_63

action_232 _ = happyReduce_64

action_233 _ = happyReduce_65

action_234 _ = happyReduce_66

action_235 _ = happyReduce_67

action_236 _ = happyReduce_68

action_237 (88) = happyShift action_218
action_237 (89) = happyShift action_219
action_237 (90) = happyShift action_220
action_237 (91) = happyShift action_221
action_237 (92) = happyShift action_222
action_237 (93) = happyShift action_223
action_237 (94) = happyShift action_224
action_237 (95) = happyShift action_225
action_237 (96) = happyShift action_226
action_237 (97) = happyShift action_227
action_237 (98) = happyShift action_228
action_237 (99) = happyShift action_229
action_237 (100) = happyShift action_230
action_237 (101) = happyShift action_231
action_237 (102) = happyShift action_232
action_237 (103) = happyShift action_233
action_237 (104) = happyShift action_234
action_237 (105) = happyShift action_235
action_237 (106) = happyShift action_236
action_237 (29) = happyGoto action_244
action_237 (30) = happyGoto action_215
action_237 (31) = happyGoto action_216
action_237 (32) = happyGoto action_217
action_237 _ = happyFail (happyExpListPerState 237)

action_238 _ = happyReduce_42

action_239 (119) = happyShift action_139
action_239 (34) = happyGoto action_243
action_239 _ = happyReduce_72

action_240 _ = happyReduce_33

action_241 _ = happyReduce_30

action_242 _ = happyReduce_38

action_243 (112) = happyShift action_250
action_243 _ = happyFail (happyExpListPerState 243)

action_244 _ = happyReduce_45

action_245 (88) = happyShift action_218
action_245 (89) = happyShift action_219
action_245 (90) = happyShift action_220
action_245 (91) = happyShift action_221
action_245 (92) = happyShift action_222
action_245 (93) = happyShift action_223
action_245 (94) = happyShift action_224
action_245 (95) = happyShift action_225
action_245 (96) = happyShift action_226
action_245 (97) = happyShift action_227
action_245 (98) = happyShift action_228
action_245 (99) = happyShift action_229
action_245 (100) = happyShift action_230
action_245 (101) = happyShift action_231
action_245 (102) = happyShift action_232
action_245 (103) = happyShift action_233
action_245 (104) = happyShift action_234
action_245 (105) = happyShift action_235
action_245 (106) = happyShift action_236
action_245 (29) = happyGoto action_249
action_245 (30) = happyGoto action_215
action_245 (31) = happyGoto action_216
action_245 (32) = happyGoto action_217
action_245 _ = happyFail (happyExpListPerState 245)

action_246 (80) = happyShift action_153
action_246 (110) = happyShift action_183
action_246 (111) = happyShift action_25
action_246 (115) = happyShift action_154
action_246 (117) = happyShift action_155
action_246 (122) = happyShift action_27
action_246 (123) = happyShift action_156
action_246 (124) = happyShift action_157
action_246 (125) = happyShift action_158
action_246 (46) = happyGoto action_248
action_246 (49) = happyGoto action_182
action_246 (51) = happyGoto action_150
action_246 (52) = happyGoto action_151
action_246 (53) = happyGoto action_152
action_246 _ = happyReduce_98

action_247 _ = happyReduce_120

action_248 (107) = happyShift action_58
action_248 (67) = happyGoto action_252
action_248 (68) = happyGoto action_56
action_248 (69) = happyGoto action_57
action_248 _ = happyReduce_141

action_249 _ = happyReduce_47

action_250 (117) = happyShift action_123
action_250 (128) = happyShift action_29
action_250 (58) = happyGoto action_121
action_250 (66) = happyGoto action_251
action_250 _ = happyFail (happyExpListPerState 250)

action_251 (107) = happyShift action_58
action_251 (110) = happyShift action_183
action_251 (67) = happyGoto action_253
action_251 (68) = happyGoto action_56
action_251 (69) = happyGoto action_57
action_251 _ = happyReduce_141

action_252 _ = happyReduce_75

action_253 _ = happyReduce_34

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (Document (pure happy_var_1)
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (coerce (happy_var_1 : coerce happy_var_2)
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn6
		 (Left happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (Right (TyDefinition happy_var_1)
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (DefinitionTypeSystem happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn7
		 (DefinitionExecutable happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (TypeSystemDefinitionSchema happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn8
		 (TypeSystemDefinitionType happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn8
		 (TypeSystemDefinitionDirective happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happyReduce 6 9 happyReduction_11
happyReduction_11 ((HappyTerminal (TokSymbol (Loc happy_var_6 SymCurlyClose))) `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn67  happy_var_3) `HappyStk`
	(HappyTerminal (TokIdentifier (Loc happy_var_2 "schema"))) `HappyStk`
	(HappyAbsSyn70  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (SchemaDefinition ((fromMaybe happy_var_2 (fmap locate happy_var_1)) <> locate happy_var_6) (fmap unLoc happy_var_1) (fmap unLoc happy_var_3) happy_var_5
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (RootOperationTypesDefinition happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (pure happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  11 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 NE.<| happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  12 happyReduction_15
happyReduction_15 (HappyAbsSyn58  happy_var_3)
	_
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn12
		 (RootOperationTypeDefinition (locate happy_var_1 <> locate happy_var_3) (unLoc happy_var_1) (unLoc happy_var_3)
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  13 happyReduction_16
happyReduction_16 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (STDef happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  13 happyReduction_17
happyReduction_17 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn13
		 (OTDef happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  13 happyReduction_18
happyReduction_18 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn13
		 (ITDef happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  13 happyReduction_19
happyReduction_19 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn13
		 (UTDef happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn13
		 (ETDef happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  13 happyReduction_21
happyReduction_21 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn13
		 (IOTDef happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happyReduce 4 14 happyReduction_22
happyReduction_22 ((HappyAbsSyn67  happy_var_4) `HappyStk`
	(HappyAbsSyn58  happy_var_3) `HappyStk`
	(HappyTerminal (TokIdentifier (Loc happy_var_2 "scalar"))) `HappyStk`
	(HappyAbsSyn70  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (ScalarTypeDefinition (fromMaybe (locate happy_var_2) (fmap locate happy_var_1) <> maybeLoc happy_var_3 happy_var_4) (fmap unLoc happy_var_1) (unLoc happy_var_3) (fmap unLoc happy_var_4)
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 6 15 happyReduction_23
happyReduction_23 ((HappyAbsSyn19  happy_var_6) `HappyStk`
	(HappyAbsSyn67  happy_var_5) `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	(HappyAbsSyn58  happy_var_3) `HappyStk`
	(HappyTerminal (TokIdentifier (Loc happy_var_2 "type"))) `HappyStk`
	(HappyAbsSyn70  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (ObjectTypeDefinition (fromMaybe (locate happy_var_2) (fmap locate happy_var_1) <> maybeLoc (maybeLoc (maybeLoc happy_var_3 happy_var_4) happy_var_5) happy_var_6) (fmap unLoc happy_var_1) (unLoc happy_var_3) happy_var_4 (fmap unLoc happy_var_5) happy_var_6
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 6 16 happyReduction_24
happyReduction_24 ((HappyAbsSyn19  happy_var_6) `HappyStk`
	(HappyAbsSyn67  happy_var_5) `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	(HappyAbsSyn58  happy_var_3) `HappyStk`
	(HappyTerminal (TokIdentifier (Loc happy_var_2 "interface"))) `HappyStk`
	(HappyAbsSyn70  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (InterfaceTypeDefinition (fromMaybe (locate happy_var_2) (fmap locate happy_var_1) <> maybeLoc (maybeLoc (maybeLoc happy_var_3 happy_var_4) happy_var_5) happy_var_6) (fmap unLoc happy_var_1) (unLoc happy_var_3) happy_var_4 (fmap unLoc happy_var_5) happy_var_6
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_1  17 happyReduction_25
happyReduction_25 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (Just (ImplementsInterfaces (locate happy_var_1) (unLoc happy_var_1))
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  17 happyReduction_26
happyReduction_26  =  HappyAbsSyn17
		 (Nothing
	)

happyReduce_27 = happySpecReduce_3  18 happyReduction_27
happyReduction_27 (HappyAbsSyn58  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (Loc (locate happy_var_1 <> locate happy_var_3) (unLoc happy_var_1 <> pure (unLoc happy_var_3))
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  18 happyReduction_28
happyReduction_28 (HappyAbsSyn58  happy_var_3)
	_
	(HappyTerminal (TokIdentifier (Loc happy_var_1 "implements")))
	 =  HappyAbsSyn18
		 (Loc (locate happy_var_1 <> locate happy_var_3) (pure (unLoc happy_var_3))
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  18 happyReduction_29
happyReduction_29 (HappyAbsSyn58  happy_var_2)
	(HappyTerminal (TokIdentifier (Loc happy_var_1 "implements")))
	 =  HappyAbsSyn18
		 (Loc (locate happy_var_1 <> locate happy_var_2) (pure (unLoc happy_var_2))
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  19 happyReduction_30
happyReduction_30 (HappyTerminal (TokSymbol (Loc happy_var_3 SymCurlyClose)))
	(HappyAbsSyn20  happy_var_2)
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymCurlyOpen)))
	 =  HappyAbsSyn19
		 (Just (FieldsDefinition (locate happy_var_1 <> locate happy_var_3) happy_var_2)
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_0  19 happyReduction_31
happyReduction_31  =  HappyAbsSyn19
		 (Nothing
	)

happyReduce_32 = happySpecReduce_1  20 happyReduction_32
happyReduction_32 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (pure happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  20 happyReduction_33
happyReduction_33 (HappyAbsSyn20  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 NE.<| happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happyReduce 6 21 happyReduction_34
happyReduction_34 ((HappyAbsSyn67  happy_var_6) `HappyStk`
	(HappyAbsSyn66  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_3) `HappyStk`
	(HappyAbsSyn58  happy_var_2) `HappyStk`
	(HappyAbsSyn70  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (FieldDefinition (fromMaybe (locate happy_var_2) (fmap locate happy_var_1) <> maybeLoc happy_var_5 happy_var_6) (fmap unLoc happy_var_1) (unLoc happy_var_2) happy_var_3 happy_var_5 (fmap unLoc happy_var_6)
	) `HappyStk` happyRest

happyReduce_35 = happyReduce 6 22 happyReduction_35
happyReduction_35 ((HappyAbsSyn23  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn67  happy_var_4) `HappyStk`
	(HappyAbsSyn58  happy_var_3) `HappyStk`
	(HappyTerminal (TokIdentifier (Loc happy_var_2 "union"))) `HappyStk`
	(HappyAbsSyn70  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (UnionTypeDefinition (fromMaybe (locate happy_var_2) (fmap locate happy_var_1) <> locate happy_var_6) (fmap unLoc happy_var_1) (unLoc happy_var_3) (fmap unLoc happy_var_4) (unLoc happy_var_6)
	) `HappyStk` happyRest

happyReduce_36 = happyReduce 7 22 happyReduction_36
happyReduction_36 ((HappyAbsSyn23  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn67  happy_var_4) `HappyStk`
	(HappyAbsSyn58  happy_var_3) `HappyStk`
	(HappyTerminal (TokIdentifier (Loc happy_var_2 "union"))) `HappyStk`
	(HappyAbsSyn70  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (UnionTypeDefinition (fromMaybe (locate happy_var_2) (fmap locate happy_var_1) <> locate happy_var_7) (fmap unLoc happy_var_1) (unLoc happy_var_3) (fmap unLoc happy_var_4) (unLoc happy_var_7)
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_1  23 happyReduction_37
happyReduction_37 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn23
		 (Loc (locate happy_var_1) [unLoc happy_var_1]
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  23 happyReduction_38
happyReduction_38 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn23
		 (Loc (locate happy_var_1 <> locate happy_var_3) (unLoc happy_var_1 : (unLoc happy_var_3))
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happyReduce 7 24 happyReduction_39
happyReduction_39 ((HappyTerminal (TokSymbol (Loc happy_var_7 SymCurlyClose))) `HappyStk`
	(HappyAbsSyn25  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn67  happy_var_4) `HappyStk`
	(HappyAbsSyn58  happy_var_3) `HappyStk`
	(HappyTerminal (TokIdentifier (Loc happy_var_2 "enum"))) `HappyStk`
	(HappyAbsSyn70  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (EnumTypeDefinition (fromMaybe (locate happy_var_2) (fmap locate happy_var_1) <> locate happy_var_7) (fmap unLoc happy_var_1) (unLoc happy_var_3) (fmap unLoc happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_1  25 happyReduction_40
happyReduction_40 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 ([happy_var_1]
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  25 happyReduction_41
happyReduction_41 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1 : happy_var_2
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  26 happyReduction_42
happyReduction_42 (HappyAbsSyn67  happy_var_3)
	(HappyAbsSyn58  happy_var_2)
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn26
		 (EnumValueDefinition (fromMaybe (locate happy_var_2) (fmap locate happy_var_1) <> maybeLoc happy_var_2 happy_var_3) (fmap unLoc happy_var_1) (unLoc happy_var_2) (fmap unLoc happy_var_3)
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happyReduce 5 27 happyReduction_43
happyReduction_43 ((HappyAbsSyn33  happy_var_5) `HappyStk`
	(HappyAbsSyn67  happy_var_4) `HappyStk`
	(HappyAbsSyn58  happy_var_3) `HappyStk`
	(HappyTerminal (TokIdentifier (Loc happy_var_2 "input"))) `HappyStk`
	(HappyAbsSyn70  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (InputObjectTypeDefinition (fromMaybe (locate happy_var_2) (fmap locate happy_var_1) <> maybeLoc (maybeLoc happy_var_3 happy_var_4) happy_var_5) (fmap unLoc happy_var_1) (unLoc happy_var_3) (fmap unLoc happy_var_4) happy_var_5
	) `HappyStk` happyRest

happyReduce_44 = happyReduce 8 28 happyReduction_44
happyReduction_44 ((HappyAbsSyn29  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_5) `HappyStk`
	(HappyTerminal (TokDirective happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokIdentifier (Loc happy_var_2 "directive"))) `HappyStk`
	(HappyAbsSyn70  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (DirectiveDefinition (fromMaybe (locate happy_var_2) (fmap locate happy_var_1) <> locate happy_var_8) (fmap unLoc happy_var_1) (Name $ unLoc happy_var_4) happy_var_5 (unLoc happy_var_8)
	) `HappyStk` happyRest

happyReduce_45 = happyReduce 9 28 happyReduction_45
happyReduction_45 ((HappyAbsSyn29  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_5) `HappyStk`
	(HappyTerminal (TokDirective happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokIdentifier (Loc happy_var_2 "directive"))) `HappyStk`
	(HappyAbsSyn70  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (DirectiveDefinition (fromMaybe (locate happy_var_2) (fmap locate happy_var_1) <> locate happy_var_9) (fmap unLoc happy_var_1) (Name $ unLoc happy_var_4) happy_var_5 (unLoc happy_var_9)
	) `HappyStk` happyRest

happyReduce_46 = happySpecReduce_1  29 happyReduction_46
happyReduction_46 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 (Loc (locate happy_var_1) [happy_var_1]
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  29 happyReduction_47
happyReduction_47 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 (Loc (locate happy_var_1 <> locate happy_var_3) (happy_var_1 : unLoc happy_var_3)
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  30 happyReduction_48
happyReduction_48 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn30
		 (ExecDirLoc (locate happy_var_1) (unLoc happy_var_1)
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  30 happyReduction_49
happyReduction_49 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn30
		 (TypeSysDirLoc (locate happy_var_1) (unLoc happy_var_1)
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  31 happyReduction_50
happyReduction_50 (HappyTerminal (TokIdentifier (Loc happy_var_1 "QUERY")))
	 =  HappyAbsSyn31
		 (Loc (locate happy_var_1) EDLQUERY
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  31 happyReduction_51
happyReduction_51 (HappyTerminal (TokIdentifier (Loc happy_var_1 "MUTATION")))
	 =  HappyAbsSyn31
		 (Loc (locate happy_var_1) EDLMUTATION
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  31 happyReduction_52
happyReduction_52 (HappyTerminal (TokIdentifier (Loc happy_var_1 "SUBSCRIPTION")))
	 =  HappyAbsSyn31
		 (Loc (locate happy_var_1) EDLSUBSCRIPTION
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  31 happyReduction_53
happyReduction_53 (HappyTerminal (TokIdentifier (Loc happy_var_1 "FIELD")))
	 =  HappyAbsSyn31
		 (Loc (locate happy_var_1) EDLFIELD
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  31 happyReduction_54
happyReduction_54 (HappyTerminal (TokIdentifier (Loc happy_var_1 "FRAGMENT_DEFINITION")))
	 =  HappyAbsSyn31
		 (Loc (locate happy_var_1) EDLFRAGMENT_DEFINITION
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  31 happyReduction_55
happyReduction_55 (HappyTerminal (TokIdentifier (Loc happy_var_1 "FRAGMENT_SPREAD")))
	 =  HappyAbsSyn31
		 (Loc (locate happy_var_1) EDLFRAGMENT_SPREAD
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  31 happyReduction_56
happyReduction_56 (HappyTerminal (TokIdentifier (Loc happy_var_1 "INLINE_FRAGMENT")))
	 =  HappyAbsSyn31
		 (Loc (locate happy_var_1) EDLINLINE_FRAGMENT
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  31 happyReduction_57
happyReduction_57 (HappyTerminal (TokIdentifier (Loc happy_var_1 "VARIABLE_DEFINITION")))
	 =  HappyAbsSyn31
		 (Loc (locate happy_var_1) EDLVARIABLE_DEFINITION
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  32 happyReduction_58
happyReduction_58 (HappyTerminal (TokIdentifier (Loc happy_var_1 "SCHEMA")))
	 =  HappyAbsSyn32
		 (Loc (locate happy_var_1) TSDLSCHEMA
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  32 happyReduction_59
happyReduction_59 (HappyTerminal (TokIdentifier (Loc happy_var_1 "SCALAR")))
	 =  HappyAbsSyn32
		 (Loc (locate happy_var_1) TSDLSCALAR
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  32 happyReduction_60
happyReduction_60 (HappyTerminal (TokIdentifier (Loc happy_var_1 "OBJECT")))
	 =  HappyAbsSyn32
		 (Loc (locate happy_var_1) TSDLOBJECT
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  32 happyReduction_61
happyReduction_61 (HappyTerminal (TokIdentifier (Loc happy_var_1 "FIELD_DEFINITION")))
	 =  HappyAbsSyn32
		 (Loc (locate happy_var_1) TSDLFIELD_DEFINITION
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  32 happyReduction_62
happyReduction_62 (HappyTerminal (TokIdentifier (Loc happy_var_1 "ARGUMENT_DEFINITION")))
	 =  HappyAbsSyn32
		 (Loc (locate happy_var_1) TSDLARGUMENT_DEFINITION
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  32 happyReduction_63
happyReduction_63 (HappyTerminal (TokIdentifier (Loc happy_var_1 "INTERFACE")))
	 =  HappyAbsSyn32
		 (Loc (locate happy_var_1) TSDLINTERFACE
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  32 happyReduction_64
happyReduction_64 (HappyTerminal (TokIdentifier (Loc happy_var_1 "UNION")))
	 =  HappyAbsSyn32
		 (Loc (locate happy_var_1) TSDLUNION
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  32 happyReduction_65
happyReduction_65 (HappyTerminal (TokIdentifier (Loc happy_var_1 "ENUM")))
	 =  HappyAbsSyn32
		 (Loc (locate happy_var_1) TSDLENUM
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  32 happyReduction_66
happyReduction_66 (HappyTerminal (TokIdentifier (Loc happy_var_1 "ENUM_VALUE")))
	 =  HappyAbsSyn32
		 (Loc (locate happy_var_1) TSDLENUM_VALUE
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  32 happyReduction_67
happyReduction_67 (HappyTerminal (TokIdentifier (Loc happy_var_1 "INPUT_OBJECT")))
	 =  HappyAbsSyn32
		 (Loc (locate happy_var_1) TSDLINPUT_OBJECT
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  32 happyReduction_68
happyReduction_68 (HappyTerminal (TokIdentifier (Loc happy_var_1 "INPUT_FIELD_DEFINITION")))
	 =  HappyAbsSyn32
		 (Loc (locate happy_var_1) TSDLINPUT_FIELD_DEFINITION
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  33 happyReduction_69
happyReduction_69 (HappyTerminal (TokSymbol (Loc happy_var_3 SymCurlyClose)))
	(HappyAbsSyn35  happy_var_2)
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymCurlyOpen)))
	 =  HappyAbsSyn33
		 (Just (InputFieldsDefinition (locate happy_var_1 <> locate happy_var_3) happy_var_2)
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_0  33 happyReduction_70
happyReduction_70  =  HappyAbsSyn33
		 (Nothing
	)

happyReduce_71 = happySpecReduce_3  34 happyReduction_71
happyReduction_71 _
	(HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn34
		 (Just (ArgumentsDefinition happy_var_2)
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_0  34 happyReduction_72
happyReduction_72  =  HappyAbsSyn34
		 (Nothing
	)

happyReduce_73 = happySpecReduce_1  35 happyReduction_73
happyReduction_73 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1 NE.:| []
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_2  35 happyReduction_74
happyReduction_74 (HappyAbsSyn35  happy_var_2)
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1 NE.<| happy_var_2
	)
happyReduction_74 _ _  = notHappyAtAll 

happyReduce_75 = happyReduce 6 36 happyReduction_75
happyReduction_75 ((HappyAbsSyn67  happy_var_6) `HappyStk`
	(HappyAbsSyn46  happy_var_5) `HappyStk`
	(HappyAbsSyn66  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn58  happy_var_2) `HappyStk`
	(HappyAbsSyn70  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn36
		 (InputValueDefinition (maybeLoc happy_var_2 happy_var_1 <> maybeLoc (maybeLoc happy_var_4 happy_var_5) happy_var_6) (fmap unLoc happy_var_1) (unLoc happy_var_2) happy_var_4 happy_var_5 (fmap unLoc happy_var_6)
	) `HappyStk` happyRest

happyReduce_76 = happySpecReduce_1  37 happyReduction_76
happyReduction_76 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn37
		 (ExecutableDefinitionOperation (OperationDefinitionTyped happy_var_1)
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  37 happyReduction_77
happyReduction_77 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn37
		 (ExecutableDefinitionOperation (OperationDefinitionUnTyped happy_var_1 )
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  37 happyReduction_78
happyReduction_78 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn37
		 (ExecutableDefinitionFragment happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  38 happyReduction_79
happyReduction_79 (HappyAbsSyn43  happy_var_3)
	(HappyAbsSyn67  happy_var_2)
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn38
		 (TypedOperationDefinition (locate happy_var_1 <> locate happy_var_3 )(unLoc happy_var_1) Nothing mempty (fmap unLoc happy_var_2) (unLoc happy_var_3)
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happyReduce 4 38 happyReduction_80
happyReduction_80 ((HappyAbsSyn43  happy_var_4) `HappyStk`
	(HappyAbsSyn67  happy_var_3) `HappyStk`
	(HappyAbsSyn58  happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (TypedOperationDefinition (locate happy_var_1 <> locate happy_var_4) (unLoc happy_var_1) (Just (unLoc happy_var_2)) mempty (fmap unLoc happy_var_3) (unLoc happy_var_4)
	) `HappyStk` happyRest

happyReduce_81 = happyReduce 4 38 happyReduction_81
happyReduction_81 ((HappyAbsSyn43  happy_var_4) `HappyStk`
	(HappyAbsSyn67  happy_var_3) `HappyStk`
	(HappyAbsSyn62  happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (TypedOperationDefinition (locate happy_var_1 <> locate happy_var_4) (unLoc happy_var_1) Nothing (Just happy_var_2) (fmap unLoc happy_var_3) (unLoc happy_var_4)
	) `HappyStk` happyRest

happyReduce_82 = happyReduce 5 38 happyReduction_82
happyReduction_82 ((HappyAbsSyn43  happy_var_5) `HappyStk`
	(HappyAbsSyn67  happy_var_4) `HappyStk`
	(HappyAbsSyn62  happy_var_3) `HappyStk`
	(HappyAbsSyn58  happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (TypedOperationDefinition (locate happy_var_1 <> locate happy_var_5) (unLoc happy_var_1) (Just (unLoc happy_var_2)) (Just happy_var_3) (fmap unLoc happy_var_4) (unLoc happy_var_5)
	) `HappyStk` happyRest

happyReduce_83 = happyReduce 5 39 happyReduction_83
happyReduction_83 ((HappyAbsSyn43  happy_var_5) `HappyStk`
	(HappyAbsSyn67  happy_var_4) `HappyStk`
	(HappyAbsSyn65  happy_var_3) `HappyStk`
	(HappyAbsSyn58  happy_var_2) `HappyStk`
	(HappyTerminal (TokIdentifier (Loc happy_var_1 "fragment"))) `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (FragmentDefinition (locate happy_var_1 <> locate happy_var_5) (unLoc happy_var_2) happy_var_3 (fmap unLoc happy_var_4) (unLoc happy_var_5)
	) `HappyStk` happyRest

happyReduce_84 = happySpecReduce_3  40 happyReduction_84
happyReduction_84 (HappyAbsSyn67  happy_var_3)
	(HappyAbsSyn58  happy_var_2)
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymSpread)))
	 =  HappyAbsSyn40
		 (FragmentSpread (locate happy_var_1 <> maybeLoc happy_var_2 happy_var_3) (unLoc happy_var_2) (fmap unLoc happy_var_3)
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happyReduce 4 41 happyReduction_85
happyReduction_85 ((HappyAbsSyn43  happy_var_4) `HappyStk`
	(HappyAbsSyn67  happy_var_3) `HappyStk`
	(HappyAbsSyn65  happy_var_2) `HappyStk`
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymSpread))) `HappyStk`
	happyRest)
	 = HappyAbsSyn41
		 (InlineFragment (locate happy_var_1 <> locate happy_var_4) (Just happy_var_2) (fmap unLoc happy_var_3) (unLoc happy_var_4)
	) `HappyStk` happyRest

happyReduce_86 = happySpecReduce_3  41 happyReduction_86
happyReduction_86 (HappyAbsSyn43  happy_var_3)
	(HappyAbsSyn67  happy_var_2)
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymSpread)))
	 =  HappyAbsSyn41
		 (InlineFragment (locate happy_var_1 <> locate happy_var_3) Nothing (fmap unLoc happy_var_2) (unLoc happy_var_3)
	)
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_2  42 happyReduction_87
happyReduction_87 (HappyAbsSyn67  happy_var_2)
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn42
		 (Field (locate happy_var_1 <> maybeLoc happy_var_1 happy_var_2) (fst (unLoc happy_var_1)) (snd (unLoc happy_var_1)) Nothing (fmap unLoc happy_var_2) mempty
	)
happyReduction_87 _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_3  42 happyReduction_88
happyReduction_88 (HappyAbsSyn67  happy_var_3)
	(HappyAbsSyn59  happy_var_2)
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn42
		 (Field (locate happy_var_1 <> maybeLoc happy_var_2 happy_var_3) (fst (unLoc happy_var_1)) (snd (unLoc happy_var_1)) (Just happy_var_2) (fmap unLoc happy_var_3) mempty
	)
happyReduction_88 _ _ _  = notHappyAtAll 

happyReduce_89 = happyReduce 4 42 happyReduction_89
happyReduction_89 ((HappyAbsSyn43  happy_var_4) `HappyStk`
	(HappyAbsSyn67  happy_var_3) `HappyStk`
	(HappyAbsSyn59  happy_var_2) `HappyStk`
	(HappyAbsSyn57  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn42
		 (Field (locate happy_var_1 <> locate happy_var_4) (fst (unLoc happy_var_1)) (snd (unLoc happy_var_1)) (Just happy_var_2) (fmap unLoc happy_var_3) (Just (unLoc happy_var_4))
	) `HappyStk` happyRest

happyReduce_90 = happySpecReduce_3  42 happyReduction_90
happyReduction_90 (HappyAbsSyn43  happy_var_3)
	(HappyAbsSyn67  happy_var_2)
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn42
		 (Field (locate happy_var_1 <> locate happy_var_3) (fst (unLoc happy_var_1)) (snd (unLoc happy_var_1)) Nothing (fmap unLoc happy_var_2) (Just (unLoc happy_var_3))
	)
happyReduction_90 _ _ _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_3  43 happyReduction_91
happyReduction_91 (HappyTerminal (TokSymbol (Loc happy_var_3 SymCurlyClose)))
	(HappyAbsSyn44  happy_var_2)
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymCurlyOpen)))
	 =  HappyAbsSyn43
		 (Loc (locate happy_var_1 <> locate happy_var_3) (SelectionSet (NE.fromList happy_var_2))
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_1  44 happyReduction_92
happyReduction_92 (HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn44
		 ([ happy_var_1 ]
	)
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_2  44 happyReduction_93
happyReduction_93 (HappyAbsSyn44  happy_var_2)
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn44
		 (happy_var_1 : happy_var_2
	)
happyReduction_93 _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  45 happyReduction_94
happyReduction_94 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn45
		 (SelectionField happy_var_1
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_1  45 happyReduction_95
happyReduction_95 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn45
		 (SelectionFragmentSpread happy_var_1
	)
happyReduction_95 _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  45 happyReduction_96
happyReduction_96 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn45
		 (SelectionInlineFragment happy_var_1
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_1  46 happyReduction_97
happyReduction_97 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn46
		 (Just happy_var_1
	)
happyReduction_97 _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_0  46 happyReduction_98
happyReduction_98  =  HappyAbsSyn46
		 (Nothing
	)

happyReduce_99 = happySpecReduce_1  47 happyReduction_99
happyReduction_99 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn47
		 ([happy_var_1]
	)
happyReduction_99 _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_2  47 happyReduction_100
happyReduction_100 (HappyAbsSyn47  happy_var_2)
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn47
		 (happy_var_1 : happy_var_2
	)
happyReduction_100 _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_1  48 happyReduction_101
happyReduction_101 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_101 _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_1  48 happyReduction_102
happyReduction_102 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn48
		 (VVar (locate happy_var_1) (unLoc happy_var_1)
	)
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_1  49 happyReduction_103
happyReduction_103 (HappyTerminal (TokIdentifier (Loc happy_var_1 "null")))
	 =  HappyAbsSyn48
		 (VNull (locate happy_var_1)
	)
happyReduction_103 _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_1  49 happyReduction_104
happyReduction_104 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn48
		 (VString (locate happy_var_1) (unLoc happy_var_1)
	)
happyReduction_104 _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_1  49 happyReduction_105
happyReduction_105 (HappyTerminal (TokNumLit _ happy_var_1))
	 =  HappyAbsSyn48
		 (VFloat (locate happy_var_1) (unLoc happy_var_1)
	)
happyReduction_105 _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_1  49 happyReduction_106
happyReduction_106 (HappyTerminal (TokIntLit happy_var_1))
	 =  HappyAbsSyn48
		 (VInt (locate happy_var_1) (unLoc happy_var_1)
	)
happyReduction_106 _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_1  49 happyReduction_107
happyReduction_107 (HappyTerminal (TokBoolLit happy_var_1))
	 =  HappyAbsSyn48
		 (VBoolean (locate happy_var_1) (unLoc happy_var_1)
	)
happyReduction_107 _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_1  49 happyReduction_108
happyReduction_108 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_108 _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_1  49 happyReduction_109
happyReduction_109 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_109 _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_2  50 happyReduction_110
happyReduction_110 (HappyTerminal (TokIdentifier happy_var_2))
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymBling)))
	 =  HappyAbsSyn50
		 (Loc (locate happy_var_1 <> locate happy_var_2) ("$" <> unLoc happy_var_2)
	)
happyReduction_110 _ _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_2  51 happyReduction_111
happyReduction_111 (HappyTerminal (TokSymbol (Loc happy_var_2 SymDoubleQuote)))
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymDoubleQuote)))
	 =  HappyAbsSyn50
		 (Loc (locate happy_var_1 <> locate happy_var_2) ""
	)
happyReduction_111 _ _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_3  51 happyReduction_112
happyReduction_112 (HappyTerminal (TokSymbol (Loc happy_var_3 SymDoubleQuote)))
	(HappyTerminal (TokStringLit happy_var_2))
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymDoubleQuote)))
	 =  HappyAbsSyn50
		 (Loc (locate happy_var_1 <> locate happy_var_3) (unLoc happy_var_2)
	)
happyReduction_112 _ _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_3  51 happyReduction_113
happyReduction_113 (HappyTerminal (TokSymbol (Loc happy_var_3 SymBlockQuote)))
	(HappyTerminal (TokStringBlock happy_var_2))
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymBlockQuote)))
	 =  HappyAbsSyn50
		 (Loc (locate happy_var_1 <> locate happy_var_3) (unLoc happy_var_2)
	)
happyReduction_113 _ _ _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_2  52 happyReduction_114
happyReduction_114 (HappyTerminal (TokSymbol (Loc happy_var_2 SymSquareClose)))
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymSquareOpen)))
	 =  HappyAbsSyn48
		 (VList (locate happy_var_1 <> locate happy_var_2) []
	)
happyReduction_114 _ _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_3  52 happyReduction_115
happyReduction_115 (HappyTerminal (TokSymbol (Loc happy_var_3 SymSquareClose)))
	(HappyAbsSyn47  happy_var_2)
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymSquareOpen)))
	 =  HappyAbsSyn48
		 (VList (locate happy_var_1 <> locate happy_var_3) happy_var_2
	)
happyReduction_115 _ _ _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_2  53 happyReduction_116
happyReduction_116 (HappyTerminal (TokSymbol (Loc happy_var_2 SymCurlyClose)))
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymCurlyOpen)))
	 =  HappyAbsSyn48
		 (VObject (locate happy_var_1 <> locate happy_var_2) mempty
	)
happyReduction_116 _ _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_3  53 happyReduction_117
happyReduction_117 (HappyTerminal (TokSymbol (Loc happy_var_3 SymCurlyClose)))
	(HappyAbsSyn54  happy_var_2)
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymCurlyOpen)))
	 =  HappyAbsSyn48
		 (VObject (locate happy_var_1 <> locate happy_var_3) happy_var_2
	)
happyReduction_117 _ _ _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_1  54 happyReduction_118
happyReduction_118 (HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn54
		 (uncurry Map.singleton happy_var_1
	)
happyReduction_118 _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_2  54 happyReduction_119
happyReduction_119 (HappyAbsSyn54  happy_var_2)
	(HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn54
		 (uncurry Map.insert happy_var_1 happy_var_2
	)
happyReduction_119 _ _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_3  55 happyReduction_120
happyReduction_120 (HappyAbsSyn48  happy_var_3)
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn55
		 ((unLoc happy_var_1, happy_var_3)
	)
happyReduction_120 _ _ _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_1  56 happyReduction_121
happyReduction_121 (HappyTerminal (TokIdentifier (Loc happy_var_1 "query")))
	 =  HappyAbsSyn56
		 (Loc (locate happy_var_1) OperationTypeQuery
	)
happyReduction_121 _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_1  56 happyReduction_122
happyReduction_122 (HappyTerminal (TokIdentifier (Loc happy_var_1 "mutation")))
	 =  HappyAbsSyn56
		 (Loc (locate happy_var_1) OperationTypeMutation
	)
happyReduction_122 _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_1  56 happyReduction_123
happyReduction_123 (HappyTerminal (TokIdentifier (Loc happy_var_1 "subscription")))
	 =  HappyAbsSyn56
		 (Loc (locate happy_var_1) OperationTypeSubscription
	)
happyReduction_123 _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_3  57 happyReduction_124
happyReduction_124 (HappyAbsSyn58  happy_var_3)
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn57
		 (Loc (locate happy_var_1 <> locate happy_var_3) (Just (unLoc happy_var_1), (unLoc happy_var_3))
	)
happyReduction_124 _ _ _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_1  57 happyReduction_125
happyReduction_125 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn57
		 (Loc (locate happy_var_1) (Nothing, (unLoc happy_var_1))
	)
happyReduction_125 _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_1  58 happyReduction_126
happyReduction_126 (HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn58
		 (Loc (locate happy_var_1) (Name (unLoc happy_var_1))
	)
happyReduction_126 _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_3  59 happyReduction_127
happyReduction_127 _
	(HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn59
		 (Arguments (locate happy_var_2) (unLoc happy_var_2)
	)
happyReduction_127 _ _ _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_1  60 happyReduction_128
happyReduction_128 (HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn60
		 (Loc (locate happy_var_1) (uncurry Map.singleton (unLoc happy_var_1))
	)
happyReduction_128 _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_2  60 happyReduction_129
happyReduction_129 (HappyAbsSyn61  happy_var_2)
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (Loc (locate happy_var_1 <> locate happy_var_2) (uncurry Map.insert (unLoc happy_var_2) (unLoc happy_var_1))
	)
happyReduction_129 _ _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_3  61 happyReduction_130
happyReduction_130 (HappyAbsSyn48  happy_var_3)
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn61
		 (Loc (locate happy_var_1 <> locate happy_var_3) (unLoc happy_var_1, happy_var_3)
	)
happyReduction_130 _ _ _  = notHappyAtAll 

happyReduce_131 = happyReduce 4 61 happyReduction_131
happyReduction_131 ((HappyAbsSyn48  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn58  happy_var_2) `HappyStk`
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymBling))) `HappyStk`
	happyRest)
	 = HappyAbsSyn61
		 (Loc (locate happy_var_1 <> locate happy_var_4) (unLoc happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_132 = happySpecReduce_3  62 happyReduction_132
happyReduction_132 _
	(HappyAbsSyn63  happy_var_2)
	_
	 =  HappyAbsSyn62
		 (VariablesDefinition happy_var_2
	)
happyReduction_132 _ _ _  = notHappyAtAll 

happyReduce_133 = happySpecReduce_1  63 happyReduction_133
happyReduction_133 (HappyAbsSyn64  happy_var_1)
	 =  HappyAbsSyn63
		 (pure happy_var_1
	)
happyReduction_133 _  = notHappyAtAll 

happyReduce_134 = happySpecReduce_2  63 happyReduction_134
happyReduction_134 (HappyAbsSyn63  happy_var_2)
	(HappyAbsSyn64  happy_var_1)
	 =  HappyAbsSyn63
		 (happy_var_1 NE.<| happy_var_2
	)
happyReduction_134 _ _  = notHappyAtAll 

happyReduce_135 = happyReduce 5 64 happyReduction_135
happyReduction_135 ((HappyAbsSyn67  happy_var_5) `HappyStk`
	(HappyAbsSyn46  happy_var_4) `HappyStk`
	(HappyAbsSyn66  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn64
		 (VariableDefinition (locate happy_var_1 <> maybeLoc (maybeLoc happy_var_3 happy_var_4) happy_var_5) (Name $ unLoc happy_var_1) happy_var_3 happy_var_4 (fmap unLoc happy_var_5)
	) `HappyStk` happyRest

happyReduce_136 = happySpecReduce_2  65 happyReduction_136
happyReduction_136 (HappyAbsSyn58  happy_var_2)
	_
	 =  HappyAbsSyn65
		 (TypeCondition (unLoc happy_var_2)
	)
happyReduction_136 _ _  = notHappyAtAll 

happyReduce_137 = happySpecReduce_1  66 happyReduction_137
happyReduction_137 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn66
		 (NamedType (locate happy_var_1) (unLoc happy_var_1)
	)
happyReduction_137 _  = notHappyAtAll 

happyReduce_138 = happySpecReduce_2  66 happyReduction_138
happyReduction_138 (HappyTerminal (TokSymbol (Loc happy_var_2 SymBang)))
	(HappyAbsSyn66  happy_var_1)
	 =  HappyAbsSyn66
		 (NonNullType (locate happy_var_1 <> locate happy_var_2) happy_var_1
	)
happyReduction_138 _ _  = notHappyAtAll 

happyReduce_139 = happySpecReduce_3  66 happyReduction_139
happyReduction_139 (HappyTerminal (TokSymbol (Loc happy_var_3 SymSquareClose)))
	(HappyAbsSyn66  happy_var_2)
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymSquareOpen)))
	 =  HappyAbsSyn66
		 (ListType (locate happy_var_1 <> locate happy_var_3) happy_var_2
	)
happyReduction_139 _ _ _  = notHappyAtAll 

happyReduce_140 = happySpecReduce_1  67 happyReduction_140
happyReduction_140 (HappyAbsSyn68  happy_var_1)
	 =  HappyAbsSyn67
		 (Just (Loc (locate happy_var_1) (Directives (unLoc happy_var_1)))
	)
happyReduction_140 _  = notHappyAtAll 

happyReduce_141 = happySpecReduce_0  67 happyReduction_141
happyReduction_141  =  HappyAbsSyn67
		 (Nothing
	)

happyReduce_142 = happySpecReduce_1  68 happyReduction_142
happyReduction_142 (HappyAbsSyn69  happy_var_1)
	 =  HappyAbsSyn68
		 (Loc (locate happy_var_1) (happy_var_1 NE.:| [])
	)
happyReduction_142 _  = notHappyAtAll 

happyReduce_143 = happySpecReduce_2  68 happyReduction_143
happyReduction_143 (HappyAbsSyn68  happy_var_2)
	(HappyAbsSyn69  happy_var_1)
	 =  HappyAbsSyn68
		 (Loc (locate happy_var_1 <> locate happy_var_2) (happy_var_1 NE.<| unLoc happy_var_2)
	)
happyReduction_143 _ _  = notHappyAtAll 

happyReduce_144 = happySpecReduce_2  69 happyReduction_144
happyReduction_144 (HappyTerminal (TokDirective happy_var_2))
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymAt)))
	 =  HappyAbsSyn69
		 (Directive (locate happy_var_1 <> locate happy_var_2) (Name $ unLoc happy_var_2) Nothing
	)
happyReduction_144 _ _  = notHappyAtAll 

happyReduce_145 = happySpecReduce_3  69 happyReduction_145
happyReduction_145 (HappyAbsSyn59  happy_var_3)
	(HappyTerminal (TokDirective happy_var_2))
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymAt)))
	 =  HappyAbsSyn69
		 (Directive (locate happy_var_1 <> locate happy_var_3) (Name $ unLoc happy_var_2) (Just happy_var_3)
	)
happyReduction_145 _ _ _  = notHappyAtAll 

happyReduce_146 = happySpecReduce_1  70 happyReduction_146
happyReduction_146 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn70
		 (Just $ Loc (locate happy_var_1) (Description (unLoc happy_var_1))
	)
happyReduction_146 _  = notHappyAtAll 

happyReduce_147 = happySpecReduce_0  70 happyReduction_147
happyReduction_147  =  HappyAbsSyn70
		 (Nothing
	)

happyReduce_148 = happySpecReduce_1  71 happyReduction_148
happyReduction_148 (HappyTerminal (TokIdentifier (Loc happy_var_1 "repeatable")))
	 =  HappyAbsSyn71
		 (Just happy_var_1
	)
happyReduction_148 _  = notHappyAtAll 

happyReduce_149 = happySpecReduce_0  71 happyReduction_149
happyReduction_149  =  HappyAbsSyn71
		 (Nothing
	)

happyNewToken action sts stk [] =
	action 130 130 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokIdentifier (Loc happy_dollar_dollar "directive") -> cont 72;
	TokIdentifier (Loc happy_dollar_dollar "enum") -> cont 73;
	TokIdentifier (Loc happy_dollar_dollar "fragment") -> cont 74;
	TokIdentifier (Loc happy_dollar_dollar "implements") -> cont 75;
	TokIdentifier (Loc happy_dollar_dollar "interface") -> cont 76;
	TokIdentifier (Loc happy_dollar_dollar "input") -> cont 77;
	TokIdentifier (Loc happy_dollar_dollar "query") -> cont 78;
	TokIdentifier (Loc happy_dollar_dollar "mutation") -> cont 79;
	TokIdentifier (Loc happy_dollar_dollar "null") -> cont 80;
	TokIdentifier (Loc happy_dollar_dollar "on") -> cont 81;
	TokIdentifier (Loc happy_dollar_dollar "repeatable") -> cont 82;
	TokIdentifier (Loc happy_dollar_dollar "scalar") -> cont 83;
	TokIdentifier (Loc happy_dollar_dollar "schema") -> cont 84;
	TokIdentifier (Loc happy_dollar_dollar "subscription") -> cont 85;
	TokIdentifier (Loc happy_dollar_dollar "type") -> cont 86;
	TokIdentifier (Loc happy_dollar_dollar "union") -> cont 87;
	TokIdentifier (Loc happy_dollar_dollar "QUERY") -> cont 88;
	TokIdentifier (Loc happy_dollar_dollar "MUTATION") -> cont 89;
	TokIdentifier (Loc happy_dollar_dollar "SUBSCRIPTION") -> cont 90;
	TokIdentifier (Loc happy_dollar_dollar "FIELD") -> cont 91;
	TokIdentifier (Loc happy_dollar_dollar "FRAGMENT_DEFINITION") -> cont 92;
	TokIdentifier (Loc happy_dollar_dollar "FRAGMENT_SPREAD") -> cont 93;
	TokIdentifier (Loc happy_dollar_dollar "INLINE_FRAGMENT") -> cont 94;
	TokIdentifier (Loc happy_dollar_dollar "VARIABLE_DEFINITION") -> cont 95;
	TokIdentifier (Loc happy_dollar_dollar "SCHEMA") -> cont 96;
	TokIdentifier (Loc happy_dollar_dollar "SCALAR") -> cont 97;
	TokIdentifier (Loc happy_dollar_dollar "OBJECT") -> cont 98;
	TokIdentifier (Loc happy_dollar_dollar "FIELD_DEFINITION") -> cont 99;
	TokIdentifier (Loc happy_dollar_dollar "ARGUMENT_DEFINITION") -> cont 100;
	TokIdentifier (Loc happy_dollar_dollar "INTERFACE") -> cont 101;
	TokIdentifier (Loc happy_dollar_dollar "UNION") -> cont 102;
	TokIdentifier (Loc happy_dollar_dollar "ENUM") -> cont 103;
	TokIdentifier (Loc happy_dollar_dollar "ENUM_VALUE") -> cont 104;
	TokIdentifier (Loc happy_dollar_dollar "INPUT_OBJECT") -> cont 105;
	TokIdentifier (Loc happy_dollar_dollar "INPUT_FIELD_DEFINITION") -> cont 106;
	TokSymbol (Loc happy_dollar_dollar SymAt) -> cont 107;
	TokSymbol (Loc happy_dollar_dollar SymBling) -> cont 108;
	TokSymbol (Loc happy_dollar_dollar SymAmpersand) -> cont 109;
	TokSymbol (Loc happy_dollar_dollar SymBang) -> cont 110;
	TokSymbol (Loc happy_dollar_dollar SymDoubleQuote) -> cont 111;
	TokSymbol (Loc happy_dollar_dollar SymColon) -> cont 112;
	TokSymbol (Loc happy_dollar_dollar SymEq) -> cont 113;
	TokSymbol (Loc happy_dollar_dollar SymPipe) -> cont 114;
	TokSymbol (Loc happy_dollar_dollar SymCurlyOpen) -> cont 115;
	TokSymbol (Loc happy_dollar_dollar SymCurlyClose) -> cont 116;
	TokSymbol (Loc happy_dollar_dollar SymSquareOpen) -> cont 117;
	TokSymbol (Loc happy_dollar_dollar SymSquareClose) -> cont 118;
	TokSymbol (Loc happy_dollar_dollar SymParenOpen) -> cont 119;
	TokSymbol (Loc happy_dollar_dollar SymParenClose) -> cont 120;
	TokSymbol (Loc happy_dollar_dollar SymSpread) -> cont 121;
	TokSymbol (Loc happy_dollar_dollar SymBlockQuote) -> cont 122;
	TokIntLit happy_dollar_dollar -> cont 123;
	TokNumLit _ happy_dollar_dollar -> cont 124;
	TokBoolLit happy_dollar_dollar -> cont 125;
	TokStringBlock happy_dollar_dollar -> cont 126;
	TokStringLit happy_dollar_dollar -> cont 127;
	TokIdentifier happy_dollar_dollar -> cont 128;
	TokDirective happy_dollar_dollar -> cont 129;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 130 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Parser a -> (a -> Parser b) -> Parser b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Parser a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Parser a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Parser a
happyError' = (\(tokens, _) -> failure tokens)
parseGraphQLDocument tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

parseName tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn58 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


failure :: [Token] -> Parser a
failure [] = do
  sp <- location
  src <- gets parseSource
  parseError $ EmptyTokenStream sp src
failure (tok:_) = do
  src <- gets parseSource
  -- TODO: fix source position capture here. I think we need the prior span.
  parseError $ UnexpectedToken tok src
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
