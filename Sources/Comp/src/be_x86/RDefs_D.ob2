<* IF NOT(TARGET_386) THEN *>
This module can be used only in TARGET_386
<* END *>
MODULE RDefs_D;
IMPORT Emit;
IMPORT D := desc386;
IMPORT CodeDef;
IMPORT ir;

CONST
    UNDEF_REG   *= D.UNDEF_REG;
    SPILLED_REG *= D.ESP;

TYPE
    Reg       *= D.Reg;
    Contents  *= ARRAY Reg OF ir.VarNum;
    NODEPTR_TYPE *= POINTER TO TREE;
    TREE         *= RECORD
                        place*:             Emit.RegPlacement;
                        a*:                 Emit.AddrMode;
                    END;

    NodeInfoType*= RECORD
                     o*:  LONGINT;  -- смещение относительно
                                   -- начала процедуры
                     sg*: CodeDef.CODE_SEGM;
                     ultimateRegContents*:  Contents; -- Состояние регистров в
                                    -- конце узлов
                     ultimateDirtyRegs*:  D.RegSet; -- Состояние регистров в
                     c*:  Contents; -- Состояние регистров в
                                    -- конце узлов
                     j*: D.Condition; -- условие перехода
                     j2*: BOOLEAN;  -- Есть ли безусловный
                                   -- переход после условного
                     l1*,
                     l2*: BOOLEAN;  -- длинные ли переходы?
                     a*:  SHORTINT; -- На какую выходную дугу
                                   -- (0 или 1) ведет первый
                                   -- переход в узле
                     l*:  ir.INT;      -- 0..3 - сколько байтов
                                   -- записали для
                                   -- выравнивания
                     lb*: Emit.LABEL;
                     ca*: Emit.LABEL;
                   END;

END RDefs_D.
