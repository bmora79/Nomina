CREATE OR REPLACE PACKAGE DAF_K_DESARROLLO_RAPIDO IS

  -- Author  : YAJAIRA BARRAGAN ARTEAGA
  -- Created : 01/06/2007 10:25:53
  -- Purpose : GENERAR AUTOMATICAMENTE LOS PAQUETES DE UNA APLICACION

  PROCEDURE DAF_P_K_VALIDA     (Pv_Owner VARCHAR2,
                                Pv_Aplicacion VARCHAR2,
                                Pv_Author     VARCHAR2,
                                Pv_MsgError OUT VARCHAR2);
  --
  PROCEDURE DAF_P_ESP_K_VALIDA (Pv_Owner VARCHAR2,
                                Pv_Aplicacion VARCHAR2,
                                Pv_MsgError OUT VARCHAR2);
  --
  FUNCTION DAF_F_NOMBRE_PARAMETRO (Pv_NombreCampo VARCHAR2) RETURN VARCHAR2;
  --
  PROCEDURE DAF_P_GENERA_SCRIPT_TB (Pv_Owner VARCHAR2,
                                    Pv_Aplicacion VARCHAR2,
                                    Pv_Author VARCHAR2,
                                    Pv_MsgError OUT VARCHAR2);
  --
  PROCEDURE DAF_P_GENERA_SCRIPT_PK (Pv_Owner VARCHAR2,
                                    Pv_Aplicacion VARCHAR2,
                                    Pv_Author VARCHAR2,
                                    Pv_MsgError OUT VARCHAR2);
  --
  PROCEDURE DAF_P_GENERA_SCRIPT_FK (Pv_Owner VARCHAR2,
                                    Pv_Aplicacion VARCHAR2,
                                    Pv_Author VARCHAR2,
                                    Pv_MsgError OUT VARCHAR2);
  --
  PROCEDURE DAF_P_GENERA_SCRIPT_NN (Pv_Owner VARCHAR2,
                                    Pv_Aplicacion VARCHAR2,
                                    Pv_Author VARCHAR2,
                                    Pv_MsgError OUT VARCHAR2);
  --
  PROCEDURE DAF_P_GENERA_SCRIPT_SY (Pv_Owner VARCHAR2,
                                    Pv_Aplicacion VARCHAR2,
                                    Pv_Author VARCHAR2,
                                    Pv_MsgError OUT VARCHAR2);
  --
  PROCEDURE DAF_P_GENERA_SCRIPT_GR (Pv_Owner VARCHAR2,
                                  Pv_Aplicacion VARCHAR2,
                                  Pv_Author VARCHAR2,
                                  Pv_MsgError OUT VARCHAR2);
  --
  PROCEDURE DAF_P_GENERA_SCRIPT_CK (Pv_Owner        VARCHAR2,
                                  Pv_Aplicacion   VARCHAR2,
                                  Pv_Author       VARCHAR2,
                                  Pv_MsgError OUT VARCHAR2);
  --
  PROCEDURE DAF_P_GENERA_SCRIPT_SQ (Pv_Owner        VARCHAR2,
                                  Pv_Aplicacion   VARCHAR2,
                                  Pv_Author       VARCHAR2,
                                  Pv_MsgError OUT VARCHAR2);
END DAF_K_DESARROLLO_RAPIDO;
/
CREATE OR REPLACE PACKAGE BODY DAF_K_DESARROLLO_RAPIDO is

  -- Author  : Lsi. Yajaira Ma. Barragan Arteaga
  -- Created : 01/06/2007
  -- Purpose : Generar automaticamente el codigo de paquetes de nuestras aplicaciones
  --           basado en el estandar utilizado por el Area de desarrollo de sistemas

  PROCEDURE DAF_P_K_VALIDA(Pv_Owner VARCHAR2,
                           Pv_Aplicacion VARCHAR2,
                           Pv_Author     VARCHAR2,
                           Pv_MsgError OUT VARCHAR2) IS

     CURSOR C_TABLAS IS
        SELECT *
        FROM ALL_TABLES
        WHERE OWNER = Pv_Owner
        AND TABLE_NAME LIKE Pv_Aplicacion||'%';

     CURSOR C_CONSTRAINT (Pv_NombreTabla VARCHAR2) IS
        SELECT CONSTRAINT_NAME FROM ALL_CONSTRAINTS where constraint_type = 'P'
        and table_name = Pv_NombreTabla
        AND OWNER = Pv_Owner;

     CURSOR C_CONSTRAINT_COLUMNS(Pv_NombreConstraint VARCHAR2) IS
        SELECT * FROM ALL_CONS_COLUMNS
        WHERE CONSTRAINT_NAME = Pv_NombreConstraint
        AND OWNER = Pv_Owner
        ORDER BY POSITION;

     CURSOR C_CONT_COLUMNS(Pv_NombreConstraint VARCHAR2) IS
        SELECT COUNT(*) FROM ALL_CONS_COLUMNS
        WHERE CONSTRAINT_NAME = Pv_NombreConstraint
        AND OWNER = Pv_Owner;

     CURSOR C_TABLE_COLUMNS(Pv_NombreTabla VARCHAR2, Pv_NombreCampo VARCHAR2) IS
        SELECT DATA_TYPE FROM ALL_TAB_COLUMNS
        WHERE TABLE_NAME = Pv_NombreTabla
        AND COLUMN_NAME = Pv_NombreCampo
        AND OWNER = Pv_Owner;

    Lr_Constraint C_CONSTRAINT%ROWTYPE;
    Lr_TableColumn C_TABLE_COLUMNS%ROWTYPE;
    Lv_NombreParametro VARCHAR2(30);
    Lv_CadenaParametros VARCHAR2(32000);

    Lv_Cabecera VARCHAR2(32000);
    Lv_Prefijo VARCHAR2(3);
    Lv_ListaParametro VARCHAR2(32000);
    Lv_NombreRegistro  VARCHAR2(32000);
    Lv_Espacios varchar2(2000);
    Lv_CabeceraCursor VARCHAR2(1000);
    Lv_CadenaCursor VARCHAR2(1000);
    Ln_Cont NUMBER;
    Lv_EspaciosCursor VARCHAR2(1000);
    Ln_Cantidad number;
    Lv_NombreTabla VARCHAR2(30);
BEGIN

     DBMS_OUTPUT.PUT_LINE ('CREATE OR REPLACE PACKAGE BODY '|| Pv_Aplicacion||'_K_VALIDA IS');
     FOR REG_TABLA IN C_TABLAS LOOP
         Lv_NombreTabla := NULL;
         Lv_NombreRegistro := NULL;
         IF(LENGTH(REG_TABLA.TABLE_NAME)<30)THEN
            FOR I IN LENGTH(REG_TABLA.TABLE_NAME) .. 29 LOOP
                Lv_NombreTabla := Lv_NombreTabla || ' ';
            END LOOP;
         END IF;
         Lv_NombreTabla := REG_TABLA.TABLE_NAME ||Lv_NombreTabla;

         Lv_Cabecera := '   PROCEDURE '||SUBSTR(Pv_Aplicacion||'_P_'||SUBSTR(Lv_NombreTabla,5,LENGTH(Lv_NombreTabla)),1,30)||'(';
         Lv_Espacios := null;
         FOR I IN 0.. LENGTH(Lv_Cabecera)-1 LOOP
             Lv_Espacios:= Lv_Espacios || ' ';
         END LOOP;

         IF C_CONSTRAINT%ISOPEN THEN
            CLOSE C_CONSTRAINT;
         END IF;
         OPEN C_CONSTRAINT(REG_TABLA.TABLE_NAME);
         FETCH C_CONSTRAINT INTO Lr_Constraint;
         CLOSE C_CONSTRAINT;
         Lv_CadenaParametros := NULL;
         --Lv_NombreRegistro := NULL;
         IF (C_CONT_COLUMNS%ISOPEN) THEN
            CLOSE C_CONT_COLUMNS;
         END IF;
         OPEN C_CONT_COLUMNS(Lr_Constraint.CONSTRAINT_NAME);
         FETCH C_CONT_COLUMNS INTO Ln_Cantidad;
         CLOSE C_CONT_COLUMNS;
         Ln_Cont := 1;
         Lv_ListaParametro := null;
         FOR REG_CONST IN C_CONSTRAINT_COLUMNS (Lr_Constraint.CONSTRAINT_NAME) LOOP

             IF C_TABLE_COLUMNS%ISOPEN THEN
                CLOSE C_TABLE_COLUMNS;
             END IF;
             OPEN C_TABLE_COLUMNS(REG_TABLA.TABLE_NAME, REG_CONST.COLUMN_NAME);
             FETCH C_TABLE_COLUMNS INTO Lr_TableColumn;
             CLOSE C_TABLE_COLUMNS;

             IF (Lr_TableColumn.DATA_TYPE = 'NUMBER') THEN
                Lv_Prefijo := 'Pn_';
             ELSIF (Lr_TableColumn.DATA_TYPE = 'DATE') THEN
                Lv_Prefijo := 'Pd_';
             ELSE
                Lv_Prefijo := 'Pv_';
             END IF;

             Lv_NombreParametro :=  Lv_Prefijo||REPLACE ((DAF_F_NOMBRE_PARAMETRO(REG_CONST.COLUMN_NAME)),'_','');
             IF(Lv_ListaParametro IS NULL) THEN
                Lv_ListaParametro := Lv_NombreParametro;
             ELSE
                Lv_ListaParametro := Lv_ListaParametro ||', ' ||Lv_NombreParametro;
             END IF;
             IF(LENGTH(Lv_NombreParametro)<30)THEN
                FOR I IN LENGTH(Lv_NombreParametro) .. 29 LOOP
                    Lv_NombreParametro := Lv_NombreParametro || ' ';
                END LOOP;
             END IF;
             IF (Ln_Cont = 1 ) THEN
                 DBMS_OUTPUT.PUT_LINE (Lv_Cabecera || Lv_NombreParametro|| ' IN  ' || Lr_TableColumn.DATA_TYPE || ',');
             ELSE
                 Lv_CadenaParametros := Lv_Espacios ||Lv_NombreParametro || ' IN  ' || Lr_TableColumn.DATA_TYPE || ',';
                 DBMS_OUTPUT.PUT_LINE (Lv_CadenaParametros);
             END IF;

             Ln_Cont := Ln_Cont + 1;

         END LOOP;
         --Lv_NombreRegistro := Lv_NombreRegistro||CHR(10);

         Lv_NombreRegistro :=  'Pr_'||DAF_F_NOMBRE_PARAMETRO(SUBSTR(REG_TABLA.TABLE_NAME,5,LENGTH(REG_TABLA.TABLE_NAME))) ;

         IF(LENGTH(Lv_NombreRegistro)<30)THEN
            FOR I IN LENGTH(Lv_NombreRegistro) .. 29 LOOP
                Lv_NombreRegistro := Lv_NombreRegistro || ' ';
            END LOOP;
         END IF;

         FOR I IN 0.. LENGTH(Lv_Cabecera)-1 LOOP
                Lv_NombreRegistro := ' '||Lv_NombreRegistro;
         END LOOP;

         Lv_NombreRegistro := Lv_NombreRegistro|| ' OUT ' || REG_TABLA.TABLE_NAME||'%ROWTYPE,' ;

         DBMS_OUTPUT.PUT_LINE (/*Lv_Cabecera || Lv_CadenaParametros ||*/ Lv_NombreRegistro);
         DBMS_OUTPUT.PUT_LINE (Lv_Espacios||'Pb_Existe                      OUT BOOLEAN,');
         DBMS_OUTPUT.PUT_LINE (Lv_Espacios||'Pv_MsgError                    OUT VARCHAR2');
         DBMS_OUTPUT.PUT_LINE (Lv_Espacios||') IS');


         DBMS_OUTPUT.PUT_LINE ('   /*');
         DBMS_OUTPUT.PUT_LINE ('   OBJETIVO:');
         DBMS_OUTPUT.PUT_LINE ('       Este procedimiento sirve para verificar la existencia de un registro en la tabla ' || REG_TABLA.TABLE_NAME);
         DBMS_OUTPUT.PUT_LINE ('       AUTOR          :    '|| CASE WHEN Pv_Author IS NULL THEN  'Lsi. Yajaira Ma. Barragan Arteaga' ELSE Pv_Author END);
         DBMS_OUTPUT.PUT_LINE ('       FECHA CREACION :    ' || REPLACE(TO_CHAR(SYSDATE,'DD-MONTH-YYYY'),' ',''));
         DBMS_OUTPUT.PUT_LINE ('       PARAMETROS:');
         --
         FOR REG_CONST IN C_CONSTRAINT_COLUMNS (Lr_Constraint.CONSTRAINT_NAME) LOOP

             IF C_TABLE_COLUMNS%ISOPEN THEN
                CLOSE C_TABLE_COLUMNS;
             END IF;
             OPEN C_TABLE_COLUMNS(REG_TABLA.TABLE_NAME, REG_CONST.COLUMN_NAME);
             FETCH C_TABLE_COLUMNS INTO Lr_TableColumn;
             CLOSE C_TABLE_COLUMNS;

             IF (Lr_TableColumn.DATA_TYPE = 'NUMBER') THEN
                Lv_Prefijo := 'Pn_';
             ELSIF (Lr_TableColumn.DATA_TYPE = 'DATE') THEN
                Lv_Prefijo := 'Pd_';
             ELSE
                Lv_Prefijo := 'Pv_';
             END IF;


             Lv_NombreParametro :=  Lv_Prefijo||REPLACE (DAF_F_NOMBRE_PARAMETRO(REG_CONST.COLUMN_NAME),'_','');
             DBMS_OUTPUT.PUT_LINE (  '                  '||Lv_NombreParametro || ' Ingresa:  Clave Primaria de la tabla CAMPO '|| REG_CONST.COLUMN_NAME);

         END LOOP;
         --
         DBMS_OUTPUT.PUT_LINE ('                  Pr_'||DAF_F_NOMBRE_PARAMETRO(SUBSTR(REG_TABLA.TABLE_NAME,5,LENGTH(REG_TABLA.TABLE_NAME)))|| '  Retorna:  El registro completo de la tabla');
         DBMS_OUTPUT.PUT_LINE ('                  Pb_Existe              Retorna:  TRUE si existe, FALSE si no existe.');
         DBMS_OUTPUT.PUT_LINE ('                  Pv_MsgError            Retorna:  El Mensaje de Error, si es que ocurriese alguno.');
         DBMS_OUTPUT.PUT_LINE (' ');
         DBMS_OUTPUT.PUT_LINE ('       MODIFICACIONES: (Por favor incluya comentarios de las modificaciones que realice)');
         DBMS_OUTPUT.PUT_LINE ('   */');

         DBMS_OUTPUT.PUT_LINE ('');
         Lv_CabeceraCursor := '      CURSOR '|| 'C_'|| SUBSTR(REG_TABLA.TABLE_NAME,5,LENGTH(REG_TABLA.TABLE_NAME))||'(';
         Lv_EspaciosCursor := NULL;
         FOR I IN 0.. LENGTH(Lv_CabeceraCursor)-1 LOOP
             Lv_EspaciosCursor:= Lv_EspaciosCursor || ' ';
         END LOOP;
         Lv_CadenaCursor := null;
         Ln_Cont := 1;

          FOR REG_CONST IN C_CONSTRAINT_COLUMNS (Lr_Constraint.CONSTRAINT_NAME) LOOP

             IF C_TABLE_COLUMNS%ISOPEN THEN
                CLOSE C_TABLE_COLUMNS;
             END IF;
             OPEN C_TABLE_COLUMNS(REG_TABLA.TABLE_NAME, REG_CONST.COLUMN_NAME);
             FETCH C_TABLE_COLUMNS INTO Lr_TableColumn;
             CLOSE C_TABLE_COLUMNS;

             IF (Lr_TableColumn.DATA_TYPE = 'NUMBER') THEN
                Lv_Prefijo := 'Cn_';
             ELSIF (Lr_TableColumn.DATA_TYPE = 'DATE') THEN
                Lv_Prefijo := 'Cd_';
             ELSE
                Lv_Prefijo := 'Cv_';
             END IF;



             Lv_NombreParametro :=  Lv_Prefijo||REPLACE ((DAF_F_NOMBRE_PARAMETRO(REG_CONST.COLUMN_NAME)),'_','');


             IF (Lv_CadenaCursor IS NULL) THEN
                Lv_CadenaCursor := Lv_CabeceraCursor||Lv_NombreParametro|| ' '|| Lr_TableColumn.DATA_TYPE;
                IF (Ln_Cantidad = 1) THEN
                   Lv_CadenaCursor := Lv_CadenaCursor||') IS';
                ELSE
                   Lv_CadenaCursor := Lv_CadenaCursor||',';
                END IF;
                DBMS_OUTPUT.PUT_LINE (Lv_CadenaCursor);
             ELSE
                DBMS_OUTPUT.PUT_LINE (Lv_EspaciosCursor|| Lv_NombreParametro || ' ' || Lr_TableColumn.DATA_TYPE ||case when Ln_Cont <>Ln_Cantidad THEN ',' ELSE ') IS' END);
             END IF;

             Ln_Cont := Ln_Cont + 1;

         END LOOP;
         DBMS_OUTPUT.PUT_LINE ('          SELECT * ');
         DBMS_OUTPUT.PUT_LINE ('            FROM ' || REG_TABLA.TABLE_NAME);
         Lv_CadenaCursor := null;
         Ln_Cont := 1;
         FOR REG_CONST IN C_CONSTRAINT_COLUMNS (Lr_Constraint.CONSTRAINT_NAME) LOOP

             IF C_TABLE_COLUMNS%ISOPEN THEN
                CLOSE C_TABLE_COLUMNS;
             END IF;
             OPEN C_TABLE_COLUMNS(REG_TABLA.TABLE_NAME, REG_CONST.COLUMN_NAME);
             FETCH C_TABLE_COLUMNS INTO Lr_TableColumn;
             CLOSE C_TABLE_COLUMNS;

             IF (Lr_TableColumn.DATA_TYPE = 'NUMBER') THEN
                Lv_Prefijo := 'Cn_';
             ELSIF (Lr_TableColumn.DATA_TYPE = 'DATE') THEN
                Lv_Prefijo := 'Cd_';
             ELSE
                Lv_Prefijo := 'Cv_';
             END IF;



             Lv_NombreParametro :=  Lv_Prefijo||REPLACE (DAF_F_NOMBRE_PARAMETRO(REG_CONST.COLUMN_NAME),'_','');
             IF (Lv_CadenaCursor IS NULL) THEN
                IF (Ln_Cantidad = 1) THEN
                   Lv_CadenaCursor := '            WHERE '|| REG_CONST.COLUMN_NAME ||' = ' || Lv_NombreParametro || ';';
                ELSE
                   Lv_CadenaCursor := '            WHERE '|| REG_CONST.COLUMN_NAME ||' = ' || Lv_NombreParametro;
                END IF;
                DBMS_OUTPUT.PUT_LINE (  Lv_CadenaCursor);
             ELSE
                DBMS_OUTPUT.PUT_LINE (  '              AND '|| REG_CONST.COLUMN_NAME ||' = ' || Lv_NombreParametro||case when Ln_Cont <>Ln_Cantidad THEN '' ELSE ';' END);
             END IF;

             Ln_Cont := Ln_Cont + 1;

         END LOOP;
         DBMS_OUTPUT.PUT_LINE ('');
         DBMS_OUTPUT.PUT_LINE ('   BEGIN');
         DBMS_OUTPUT.PUT_LINE ('');
         FOR REG_CONST IN C_CONSTRAINT_COLUMNS (Lr_Constraint.CONSTRAINT_NAME) LOOP

             IF C_TABLE_COLUMNS%ISOPEN THEN
                CLOSE C_TABLE_COLUMNS;
             END IF;
             OPEN C_TABLE_COLUMNS(REG_TABLA.TABLE_NAME, REG_CONST.COLUMN_NAME);
             FETCH C_TABLE_COLUMNS INTO Lr_TableColumn;
             CLOSE C_TABLE_COLUMNS;

             IF (Lr_TableColumn.DATA_TYPE = 'NUMBER') THEN
                Lv_Prefijo := 'Pn_';
             ELSIF (Lr_TableColumn.DATA_TYPE = 'DATE') THEN
                Lv_Prefijo := 'Pd_';
             ELSE
                Lv_Prefijo := 'Pv_';
             END IF;

             Lv_NombreParametro :=  Lv_Prefijo||REPLACE (DAF_F_NOMBRE_PARAMETRO(REG_CONST.COLUMN_NAME),'_','');

             DBMS_OUTPUT.PUT_LINE ('      IF ('||Lv_NombreParametro|| ' IS NULL) THEN');
             DBMS_OUTPUT.PUT_LINE ('         Pv_MsgError := '''|| Pv_Aplicacion||'_K_VALIDA.'||Pv_Aplicacion||'_P_'||SUBSTR(REG_TABLA.TABLE_NAME,5,LENGTH(REG_TABLA.TABLE_NAME))|| '-> Parametro incorrecto:' || Lv_NombreParametro||''';');
             DBMS_OUTPUT.PUT_LINE ('         RETURN;');
             DBMS_OUTPUT.PUT_LINE ('      END IF;');
             DBMS_OUTPUT.PUT_LINE ('');
         END LOOP;
         DBMS_OUTPUT.PUT_LINE ('');
         DBMS_OUTPUT.PUT_LINE ('      IF ('|| 'C_'  || SUBSTR(REG_TABLA.TABLE_NAME,5,LENGTH(REG_TABLA.TABLE_NAME))||'%ISOPEN) THEN');
         DBMS_OUTPUT.PUT_LINE ('         CLOSE C_'|| SUBSTR(REG_TABLA.TABLE_NAME,5,LENGTH(REG_TABLA.TABLE_NAME))||';');
         DBMS_OUTPUT.PUT_LINE ('      END IF;');

         DBMS_OUTPUT.PUT_LINE ('      OPEN C_'  || SUBSTR(REG_TABLA.TABLE_NAME,5,LENGTH(REG_TABLA.TABLE_NAME))||'('||Lv_ListaParametro||');');
         DBMS_OUTPUT.PUT_LINE ('      FETCH C_'  || SUBSTR(REG_TABLA.TABLE_NAME,5,LENGTH(REG_TABLA.TABLE_NAME))||' INTO Pr_'||LOWER(REPLACE (SUBSTR(REG_TABLA.TABLE_NAME,5,LENGTH(REG_TABLA.TABLE_NAME)),'_',''))||';');
         DBMS_OUTPUT.PUT_LINE ('      Pb_Existe := C_'  || SUBSTR(REG_TABLA.TABLE_NAME,5,LENGTH(REG_TABLA.TABLE_NAME))||'%FOUND;');
         DBMS_OUTPUT.PUT_LINE ('      CLOSE C_'  || SUBSTR(REG_TABLA.TABLE_NAME,5,LENGTH(REG_TABLA.TABLE_NAME))||';');
         DBMS_OUTPUT.PUT_LINE ('');

         DBMS_OUTPUT.PUT_LINE ('   EXCEPTION ');
         DBMS_OUTPUT.PUT_LINE ('      WHEN OTHERS THEN');
         DBMS_OUTPUT.PUT_LINE ('         Pv_MsgError := '''|| Pv_Aplicacion ||'_K_VALIDA.'||Pv_Aplicacion||'_P_'||SUBSTR(REG_TABLA.TABLE_NAME,5,LENGTH(Lv_NombreTabla))||' -> '' || TO_CHAR(SQLCODE) || '' '' || SQLERRM;');

         DBMS_OUTPUT.PUT_LINE ('   END '||SUBSTR(Pv_Aplicacion||'_P_'||SUBSTR(REG_TABLA.TABLE_NAME,5,LENGTH(REG_TABLA.TABLE_NAME)),1,30)||';');
         DBMS_OUTPUT.PUT_LINE ('   --');
         DBMS_OUTPUT.PUT_LINE ('');
     END LOOP;
     DBMS_OUTPUT.PUT_LINE ('END '|| Pv_Aplicacion||'_K_VALIDA;');
     EXCEPTION
               WHEN OTHERS THEN
                    Pv_MsgError := 'DAF_K_DESARROLLO_RAPIDO -> DAF_P_K_VALIDA ('|| SQLCODE || ') '|| SQLERRM;
END DAF_P_K_VALIDA;
--

  PROCEDURE DAF_P_ESP_K_VALIDA (Pv_Owner VARCHAR2,
                                Pv_Aplicacion VARCHAR2,
                                Pv_MsgError OUT VARCHAR2) IS
      CURSOR C_TABLAS IS
         SELECT *
         FROM ALL_TABLES
          WHERE OWNER = Pv_Owner
          AND TABLE_NAME LIKE Pv_Aplicacion||'%';

       CURSOR C_CONSTRAINT (Pv_NombreTabla VARCHAR2) IS
          SELECT CONSTRAINT_NAME FROM ALL_CONSTRAINTS where constraint_type = 'P'
          and table_name = Pv_NombreTabla
          AND OWNER = Pv_Owner;

       CURSOR C_CONSTRAINT_COLUMNS(Pv_NombreConstraint VARCHAR2) IS
          SELECT * FROM ALL_CONS_COLUMNS
          WHERE CONSTRAINT_NAME = Pv_NombreConstraint
          AND OWNER =Pv_Owner
          ORDER BY POSITION;

       CURSOR C_CONT_COLUMNS(Pv_NombreConstraint VARCHAR2) IS
          SELECT COUNT(*) FROM ALL_CONS_COLUMNS
          WHERE CONSTRAINT_NAME = Pv_NombreConstraint
          AND OWNER = Pv_Owner;


       CURSOR C_TABLE_COLUMNS(Pv_NombreTabla VARCHAR2, Pv_NombreCampo VARCHAR2) IS
          SELECT DATA_TYPE FROM ALL_TAB_COLUMNS
          WHERE TABLE_NAME = Pv_NombreTabla
          AND COLUMN_NAME = Pv_NombreCampo
          AND OWNER =Pv_Owner;

    Lr_Constraint C_CONSTRAINT%ROWTYPE;
    Lr_TableColumn C_TABLE_COLUMNS%ROWTYPE;
    Lv_NombreParametro VARCHAR2(30);
    Lv_CadenaParametros VARCHAR2(32000);

    Lv_Cabecera VARCHAR2(32000);
    Lv_Prefijo VARCHAR2(3);
    Lv_NombreRegistro  VARCHAR2(32000);
    Lv_Espacios varchar2(2000);
    Ln_Cont NUMBER;
    Ln_Cantidad number;
    Lv_NombreTabla VARCHAR2(30);
BEGIN


     DBMS_OUTPUT.PUT_LINE ('CREATE OR REPLACE PACKAGE '|| Pv_Aplicacion||'_K_VALIDA AS');
     FOR REG_TABLA IN C_TABLAS LOOP
         Lv_NombreTabla := NULL;
         IF(LENGTH(REG_TABLA.TABLE_NAME)<30)THEN
            FOR I IN LENGTH(REG_TABLA.TABLE_NAME) .. 29 LOOP
                Lv_NombreTabla := Lv_NombreTabla || ' ';
            END LOOP;
         END IF;
         Lv_NombreTabla := REG_TABLA.TABLE_NAME ||Lv_NombreTabla;

         Lv_Cabecera := '   PROCEDURE '||SUBSTR(Pv_Aplicacion||'_P_'||SUBSTR(Lv_NombreTabla,5,LENGTH(Lv_NombreTabla)),1,30)||'(';
         Lv_Espacios := null;
         FOR I IN 0.. LENGTH(Lv_Cabecera)-1 LOOP
             Lv_Espacios:= Lv_Espacios || ' ';
         END LOOP;

         IF C_CONSTRAINT%ISOPEN THEN
            CLOSE C_CONSTRAINT;
         END IF;
         OPEN C_CONSTRAINT(REG_TABLA.TABLE_NAME);
         FETCH C_CONSTRAINT INTO Lr_Constraint;
         CLOSE C_CONSTRAINT;
         Lv_CadenaParametros := NULL;
         Lv_NombreRegistro := NULL;
         IF (C_CONT_COLUMNS%ISOPEN) THEN
            CLOSE C_CONT_COLUMNS;
         END IF;
         OPEN C_CONT_COLUMNS(Lr_Constraint.CONSTRAINT_NAME);
         FETCH C_CONT_COLUMNS INTO Ln_Cantidad;
         CLOSE C_CONT_COLUMNS;
         Ln_Cont := 1;
         FOR REG_CONST IN C_CONSTRAINT_COLUMNS (Lr_Constraint.CONSTRAINT_NAME) LOOP

             IF C_TABLE_COLUMNS%ISOPEN THEN
                CLOSE C_TABLE_COLUMNS;
             END IF;
             OPEN C_TABLE_COLUMNS(REG_TABLA.TABLE_NAME, REG_CONST.COLUMN_NAME);
             FETCH C_TABLE_COLUMNS INTO Lr_TableColumn;
             CLOSE C_TABLE_COLUMNS;

             IF (Lr_TableColumn.DATA_TYPE = 'NUMBER') THEN
                Lv_Prefijo := 'Pn_';
             ELSIF (Lr_TableColumn.DATA_TYPE = 'DATE') THEN
                Lv_Prefijo := 'Pd_';
             ELSE
                Lv_Prefijo := 'Pv_';
             END IF;

             Lv_NombreParametro :=  Lv_Prefijo||REPLACE (DAF_F_NOMBRE_PARAMETRO(REG_CONST.COLUMN_NAME),'_','');
             IF(LENGTH(Lv_NombreParametro)<30)THEN
                FOR I IN LENGTH(Lv_NombreParametro) .. 29 LOOP
                    Lv_NombreParametro := Lv_NombreParametro || ' ';
                END LOOP;
             END IF;
             IF (Ln_Cont = 1 ) THEN
                DBMS_OUTPUT.PUT_LINE (Lv_Cabecera || Lv_NombreParametro|| ' IN  ' || Lr_TableColumn.DATA_TYPE || ',');
             ELSE
                 Lv_CadenaParametros := Lv_Espacios ||Lv_NombreParametro || ' IN  ' || Lr_TableColumn.DATA_TYPE || ',';
                 DBMS_OUTPUT.PUT_LINE (Lv_CadenaParametros);
             END IF;

             Ln_Cont := Ln_Cont + 1;

         END LOOP;
         --Lv_NombreRegistro := Lv_NombreRegistro||CHR(10);

         Lv_NombreRegistro :=  Lv_NombreRegistro || 'Pr_'||DAF_F_NOMBRE_PARAMETRO(SUBSTR(REG_TABLA.TABLE_NAME,5,LENGTH(REG_TABLA.TABLE_NAME))) ;

         IF(LENGTH(Lv_NombreRegistro)<30)THEN
            FOR I IN LENGTH(Lv_NombreRegistro) .. 29 LOOP
                Lv_NombreRegistro := Lv_NombreRegistro || ' ';
            END LOOP;
         END IF;

         FOR I IN 0.. LENGTH(Lv_Cabecera)-1 LOOP
                Lv_NombreRegistro := ' '||Lv_NombreRegistro;
         END LOOP;

         Lv_NombreRegistro := Lv_NombreRegistro|| ' OUT ' || REG_TABLA.TABLE_NAME||'%ROWTYPE,' ;

         DBMS_OUTPUT.PUT_LINE (/*Lv_Cabecera || Lv_CadenaParametros ||*/ Lv_NombreRegistro);
         DBMS_OUTPUT.PUT_LINE (Lv_Espacios||'Pb_Existe OUT BOOLEAN,');
         DBMS_OUTPUT.PUT_LINE (Lv_Espacios||'Pv_MsgError OUT VARCHAR2');
         DBMS_OUTPUT.PUT_LINE (Lv_Espacios||');');
         DBMS_OUTPUT.PUT_LINE ('   --');

     END LOOP;
     DBMS_OUTPUT.PUT_LINE ('END '|| Pv_Aplicacion||'_K_VALIDA;');
  EXCEPTION
     WHEN OTHERS THEN
          Pv_MsgError := 'DAF_K_DESARROLLO_RAPIDO -> DAF_P_K_VALIDA ('|| SQLCODE || ') '|| SQLERRM;
  END DAF_P_ESP_K_VALIDA;
  --
  FUNCTION DAF_F_NOMBRE_PARAMETRO (Pv_NombreCampo VARCHAR2) RETURN VARCHAR2 IS
     Ln_LongitudCampo NUMBER(3);
     Lv_NombreParametro VARCHAR2(30);
     Lv_Caracter VARCHAR2(1);
     Lb_Flag BOOLEAN:= TRUE;
  BEGIN
     Ln_LongitudCampo := LENGTH(Pv_NombreCampo);
     FOR I IN 1 .. Ln_LongitudCampo LOOP
         Lv_Caracter := SubStr(Pv_NombreCampo,I,1);
         IF (Lv_Caracter = '_') THEN
            Lb_Flag := TRUE;
         ELSE
           Lv_NombreParametro := Lv_NombreParametro || CASE WHEN Lb_Flag THEN UPPER(Lv_Caracter) ELSE LOWER(Lv_Caracter) END;
           IF Lb_Flag THEN
              Lb_Flag := FALSE;
           END IF;
         END IF;
     END LOOP;
     RETURN Lv_NombreParametro;
   END DAF_F_NOMBRE_PARAMETRO;
--
PROCEDURE DAF_P_GENERA_SCRIPT_TB (Pv_Owner VARCHAR2,
                                    Pv_Aplicacion VARCHAR2,
                                    Pv_Author VARCHAR2,
                                    Pv_MsgError OUT VARCHAR2) IS


   CURSOR C_TABLAS IS
     SELECT *
     FROM ALL_TABLES
     WHERE OWNER = Pv_Owner
     AND TABLE_NAME LIKE Pv_Aplicacion||'%';

    CURSOR C_TABLE_COLUMNS(Pv_NombreTabla VARCHAR2) IS
        SELECT * FROM ALL_TAB_COLUMNS
        WHERE TABLE_NAME = Pv_NombreTabla
        AND OWNER = Pv_Owner
        ORDER BY COLUMN_ID;

    CURSOR C_COUNT_TABLE_COLUMNS(Pv_NombreTabla VARCHAR2) IS
        SELECT COUNT(*) FROM ALL_TAB_COLUMNS
        WHERE TABLE_NAME = Pv_NombreTabla
        AND OWNER = Pv_Owner;

    Lv_NombreTabla VARCHAR2(30);
    Lv_Espacios VARCHAR2(1000);
    Ln_NumeroCampos NUMBER;
    Lv_NombreCampo VARCHAR2(30);
    Ln_Cont NUMBER:=0;
BEGIN

 DBMS_OUTPUT.PUT_LINE ('/***************************************************');
 DBMS_OUTPUT.PUT_LINE ('  SCRIPT DEL MODULO DE  '||Pv_Aplicacion);
 DBMS_OUTPUT.PUT_LINE ('                                                 ');
 DBMS_OUTPUT.PUT_LINE ('  AUTOR: '||nvl(Pv_Author,'Lsi. Yajaira Ma. Barragan Arteaga'));
 DBMS_OUTPUT.PUT_LINE ('  FECHA: '|| REPLACE(TO_CHAR(SYSDATE,'DD-MONTH-YYYY'),' ',''));
 DBMS_OUTPUT.PUT_LINE ('***************************************************/');
 DBMS_OUTPUT.PUT_LINE (' ');
 DBMS_OUTPUT.PUT_LINE ('-- CREACION DE TABLAS');
 DBMS_OUTPUT.PUT_LINE (' ');
 DBMS_OUTPUT.PUT_LINE ('SPOOL ../'||Pv_Aplicacion||'_TB_001.log');
 DBMS_OUTPUT.PUT_LINE (' ');
 FOR REG_TAB IN C_TABLAS LOOP
     Lv_NombreTabla := REG_TAB.TABLE_NAME;
     IF(LENGTH(Lv_NombreTabla)<30)THEN
        FOR I IN LENGTH(Lv_NombreTabla) .. 29 LOOP
            Lv_NombreTabla := Lv_NombreTabla || ' ';
        END LOOP;
     END IF;
     DBMS_OUTPUT.PUT_LINE ('DROP TABLE '||Lv_NombreTabla||'	   CASCADE CONSTRAINTS;');
 END LOOP;
 DBMS_OUTPUT.PUT_LINE (' ');
 Lv_Espacios := '                                             ';
 FOR REG_TAB IN C_TABLAS LOOP
     Lv_NombreTabla := REG_TAB.TABLE_NAME;
     IF(LENGTH(Lv_NombreTabla)<30)THEN
        FOR I IN LENGTH(Lv_NombreTabla) .. 29 LOOP
            Lv_NombreTabla := Lv_NombreTabla || ' ';
        END LOOP;
     END IF;

     DBMS_OUTPUT.PUT_LINE ('PROMPT CREANDO '|| REG_TAB.TABLE_NAME);
     DBMS_OUTPUT.PUT_LINE ('CREATE TABLE '||Lv_NombreTabla||'	(');

     OPEN C_COUNT_TABLE_COLUMNS(REG_TAB.TABLE_NAME);
     FETCH C_COUNT_TABLE_COLUMNS INTO Ln_NumeroCampos;
     CLOSE C_COUNT_TABLE_COLUMNS;
     Ln_Cont := 0;
     FOR REG_CAM IN C_TABLE_COLUMNS (REG_TAB.TABLE_NAME) LOOP
         Ln_Cont := Ln_Cont +1;
         Lv_NombreCampo := REG_CAM.COLUMN_NAME;
         IF(LENGTH(Lv_NombreCampo)<30)THEN
            FOR I IN LENGTH(Lv_NombreCampo) .. 29 LOOP
                Lv_NombreCampo := Lv_NombreCampo || ' ';
            END LOOP;
         END IF;
         DBMS_OUTPUT.PUT_LINE (Lv_Espacios||Lv_NombreCampo||' '||REG_CAM.DATA_TYPE|| CASE WHEN REG_CAM.DATA_TYPE = 'NUMBER' THEN
                                                                                               CASE WHEN REG_CAM.DATA_PRECISION IS NULL
                                                                                                    THEN CASE WHEN Ln_Cont <> Ln_NumeroCampos THEN ','
                                                                                                              ELSE ''
                                                                                                         END
                                                                                                    ELSE  CASE WHEN Ln_Cont <> Ln_NumeroCampos THEN '('||REG_CAM.DATA_PRECISION||CASE WHEN REG_CAM.DATA_SCALE IS NOT NULL AND REG_CAM.DATA_SCALE > 0 THEN ','||REG_CAM.DATA_SCALE|| '),' ELSE '),' END
                                                                                                              ELSE '('||REG_CAM.DATA_PRECISION|| CASE WHEN REG_CAM.DATA_SCALE IS NOT NULL AND REG_CAM.DATA_SCALE > 0 THEN ','||REG_CAM.DATA_SCALE|| ')' ELSE ')' END
                                                                                                         END
                                                                                               END
                                                                                               WHEN REG_CAM.DATA_TYPE = 'DATE' OR REG_CAM.DATA_TYPE = 'BOOLEAN' OR REG_CAM.DATA_TYPE = 'LONGRAW' THEN
                                                                                                    CASE WHEN Ln_Cont <> Ln_NumeroCampos THEN ','
                                                                                                         ELSE ''
                                                                                                    END
                                                                                           ELSE CASE WHEN Ln_Cont <> Ln_NumeroCampos THEN '('||REG_CAM.DATA_LENGTH||'),'
                                                                                                     ELSE '('||REG_CAM.DATA_LENGTH||')'
                                                                                                END
                                                                                           END);
     END LOOP;
     DBMS_OUTPUT.PUT_LINE (SuBSTR(Lv_Espacios,1,length(Lv_Espacios)-1)||')');
     DBMS_OUTPUT.PUT_LINE (SuBSTR(Lv_Espacios,1,length(Lv_Espacios)-1)||'TABLESPACE '|| Pv_Aplicacion ||'_DAT');
     DBMS_OUTPUT.PUT_LINE ('/');
     DBMS_OUTPUT.PUT_LINE (' ');
 END LOOP;
 DBMS_OUTPUT.PUT_LINE (' ');
 DBMS_OUTPUT.PUT_LINE ('SPOOL OFF');

EXCEPTION
         WHEN OTHERS THEN
              Pv_MsgError := 'DAF_K_DESARROLLO_RAPIDO->DAF_P_GENERA_SCRIPT_TB ('||SQLCODE ||') ' ||SQLERRM;
END DAF_P_GENERA_SCRIPT_TB;
--
PROCEDURE DAF_P_GENERA_SCRIPT_PK (Pv_Owner        VARCHAR2,
                                  Pv_Aplicacion   VARCHAR2,
                                  Pv_Author       VARCHAR2,
                                  Pv_MsgError OUT VARCHAR2) IS
  CURSOR C_CONSTRAINTS IS
         SELECT *
         FROM ALL_CONSTRAINTS
         WHERE owner = Pv_Owner
         AND TABLE_NAME LIKE Pv_Aplicacion||'_%'
         AND GENERATED = 'USER NAME'
         AND CONSTRAINT_NAME LIKE '%$P';

  CURSOR C_CONSTRAINT_COLUMNS(Cv_NombreConstraint VARCHAR2) IS
         SELECT * FROM ALL_CONS_COLUMNS
         WHERE CONSTRAINT_NAME = Cv_NombreConstraint
         AND OWNER = Pv_Owner
         ORDER BY POSITION;

  Lv_ListaCampos VARCHAR2(2000);

BEGIN

  DBMS_OUTPUT.PUT_LINE ('/***************************************************');
  DBMS_OUTPUT.PUT_LINE ('  SCRIPT DEL MODULO DE  '||Pv_Aplicacion);
  DBMS_OUTPUT.PUT_LINE ('                                                 ');
  DBMS_OUTPUT.PUT_LINE ('  AUTOR: '||nvl(Pv_Author,'Lsi. Yajaira Ma. Barragan Arteaga'));
  DBMS_OUTPUT.PUT_LINE ('  FECHA: '|| REPLACE(TO_CHAR(SYSDATE,'DD-MONTH-YYYY'),' ',''));
  DBMS_OUTPUT.PUT_LINE ('***************************************************/');
  DBMS_OUTPUT.PUT_LINE (' ');
  DBMS_OUTPUT.PUT_LINE ('-- CREACION DE PRIMARY KEYS');
  DBMS_OUTPUT.PUT_LINE (' ');
  DBMS_OUTPUT.PUT_LINE ('SPOOL ../'||Pv_Aplicacion||'_PK_001.log');
  DBMS_OUTPUT.PUT_LINE (' ');

  FOR REG_CON IN C_CONSTRAINTS LOOP

      DBMS_OUTPUT.PUT_LINE ('ALTER TABLE '||REG_CON.TABLE_NAME);
      DBMS_OUTPUT.PUT_LINE ('DROP  CONSTRAINT ' ||REG_CON.CONSTRAINT_NAME ||';');
      DBMS_OUTPUT.PUT_LINE (' ');
  END LOOP;

  FOR REG_CON IN C_CONSTRAINTS LOOP
      DBMS_OUTPUT.PUT_LINE ('PROMPT CREANDO PK '||REG_CON.TABLE_NAME);

      DBMS_OUTPUT.PUT_LINE ('ALTER TABLE '||REG_CON.TABLE_NAME);
      DBMS_OUTPUT.PUT_LINE ('      ADD  ( CONSTRAINT '|| REG_CON.CONSTRAINT_NAME);

      Lv_ListaCampos := NULL;

      FOR REG_COL IN C_CONSTRAINT_COLUMNS (REG_CON.CONSTRAINT_NAME) LOOP
          Lv_ListaCampos := Lv_ListaCampos ||REG_COL.COLUMN_NAME||',';
      END LOOP;

      Lv_ListaCampos := substr(Lv_ListaCampos,1,length(Lv_ListaCampos)-1);

      DBMS_OUTPUT.PUT_LINE ('             PRIMARY KEY ('|| Lv_ListaCampos ||')');
	    DBMS_OUTPUT.PUT_LINE ('             USING INDEX TABLESPACE ' || Pv_Aplicacion||'_IDX');
      DBMS_OUTPUT.PUT_LINE ('      )');
      DBMS_OUTPUT.PUT_LINE ('/');

      DBMS_OUTPUT.PUT_LINE (' ');
  END LOOP;
  DBMS_OUTPUT.PUT_LINE ('SPOOL OFF');
EXCEPTION
         WHEN OTHERS THEN
              Pv_MsgError := 'DAF_K_DESARROLLO_RAPIDO->DAF_P_GENERA_SCRIPT_PK ('||SQLCODE ||') ' ||SQLERRM;
END DAF_P_GENERA_SCRIPT_PK;
--
PROCEDURE DAF_P_GENERA_SCRIPT_FK (Pv_Owner        VARCHAR2,
                                  Pv_Aplicacion   VARCHAR2,
                                  Pv_Author       VARCHAR2,
                                  Pv_MsgError OUT VARCHAR2) IS
  CURSOR C_CONSTRAINTS IS
         SELECT *
         FROM ALL_CONSTRAINTS
         WHERE owner = Pv_Owner
         AND TABLE_NAME LIKE Pv_Aplicacion||'_%'
         AND GENERATED = 'USER NAME'
         AND CONSTRAINT_TYPE='R'
         ORDER BY CONSTRAINT_NAME;

  CURSOR C_CONSTRAINT_COLUMNS(Cv_NombreConstraint VARCHAR2) IS
         SELECT * FROM ALL_CONS_COLUMNS
         WHERE CONSTRAINT_NAME = Cv_NombreConstraint
         AND OWNER = Pv_Owner
         ORDER BY POSITION;

  CURSOR C_CONSTRAINT (Pv_ContraintName VARCHAR2)IS
         SELECT TABLE_NAME
         FROM ALL_CONSTRAINTS
         WHERE owner = Pv_Owner
         AND GENERATED = 'USER NAME'
         AND CONSTRAINT_NAME = Pv_ContraintName;

  Lv_ListaCampos VARCHAR2(2000);
  Lv_ListaCamposR VARCHAR2(2000);
  Lv_TablaRef VARCHAR2(30);
BEGIN

  DBMS_OUTPUT.PUT_LINE ('/***************************************************');
  DBMS_OUTPUT.PUT_LINE ('  SCRIPT DEL MODULO DE  '||Pv_Aplicacion);
  DBMS_OUTPUT.PUT_LINE ('                                                 ');
  DBMS_OUTPUT.PUT_LINE ('  AUTOR: '||nvl(Pv_Author,'Lsi. Yajaira Ma. Barragan Arteaga'));
  DBMS_OUTPUT.PUT_LINE ('  FECHA: '|| REPLACE(TO_CHAR(SYSDATE,'DD-MONTH-YYYY'),' ',''));
  DBMS_OUTPUT.PUT_LINE ('***************************************************/');
  DBMS_OUTPUT.PUT_LINE (' ');
  DBMS_OUTPUT.PUT_LINE ('-- CREACION DE FOREIGN KEYS');
  DBMS_OUTPUT.PUT_LINE (' ');
  DBMS_OUTPUT.PUT_LINE ('SPOOL ../'||Pv_Aplicacion||'_FK_001.log');
  DBMS_OUTPUT.PUT_LINE (' ');

  FOR REG_CON IN C_CONSTRAINTS LOOP
      DBMS_OUTPUT.PUT_LINE ('ALTER TABLE '||REG_CON.TABLE_NAME);
      DBMS_OUTPUT.PUT_LINE ('DROP  CONSTRAINT ' ||REG_CON.CONSTRAINT_NAME ||';');
      DBMS_OUTPUT.PUT_LINE (' ');
  END LOOP;

  FOR REG_CON IN C_CONSTRAINTS LOOP
      DBMS_OUTPUT.PUT_LINE ('PROMPT CREANDO FK'||SUBSTR(REG_CON.CONSTRAINT_NAME,LENGTH(REG_CON.CONSTRAINT_NAME),LENGTH(REG_CON.CONSTRAINT_NAME)));
      DBMS_OUTPUT.PUT_LINE ('ALTER TABLE '||REG_CON.TABLE_NAME);
      DBMS_OUTPUT.PUT_LINE ('      ADD  ( CONSTRAINT '|| REG_CON.CONSTRAINT_NAME);

      Lv_ListaCampos := NULL;

      FOR REG_COL IN C_CONSTRAINT_COLUMNS (REG_CON.CONSTRAINT_NAME) LOOP
          Lv_ListaCampos := Lv_ListaCampos ||REG_COL.COLUMN_NAME||',';
      END LOOP;

      Lv_ListaCampos := substr(Lv_ListaCampos,1,length(Lv_ListaCampos)-1);


      Lv_ListaCamposR := NULL;

      FOR REG_COL IN C_CONSTRAINT_COLUMNS (REG_CON.R_CONSTRAINT_NAME) LOOP
          Lv_ListaCamposR := Lv_ListaCamposR ||REG_COL.COLUMN_NAME||',';
      END LOOP;

      Lv_ListaCamposR := substr(Lv_ListaCamposR,1,length(Lv_ListaCamposR)-1);

      OPEN C_CONSTRAINT(REG_CON.R_CONSTRAINT_NAME);
      FETCH C_CONSTRAINT INTO Lv_TablaRef;
      CLOSE C_CONSTRAINT;

      DBMS_OUTPUT.PUT_LINE ('             FOREIGN KEY ('|| Lv_ListaCampos ||')');
	    DBMS_OUTPUT.PUT_LINE ('             REFERENCES ' ||Lv_TablaRef ||'(' || Lv_ListaCamposR||')');
      DBMS_OUTPUT.PUT_LINE ('      )');
      DBMS_OUTPUT.PUT_LINE ('/');

      DBMS_OUTPUT.PUT_LINE (' ');
  END LOOP;
  DBMS_OUTPUT.PUT_LINE ('SPOOL OFF');
EXCEPTION
         WHEN OTHERS THEN
              Pv_MsgError := 'DAF_K_DESARROLLO_RAPIDO->DAF_P_GENERA_SCRIPT_FK ('||SQLCODE ||') ' ||SQLERRM;
END DAF_P_GENERA_SCRIPT_FK;
--
PROCEDURE DAF_P_GENERA_SCRIPT_NN (Pv_Owner        VARCHAR2,
                                  Pv_Aplicacion   VARCHAR2,
                                  Pv_Author       VARCHAR2,
                                  Pv_MsgError OUT VARCHAR2) IS
 CURSOR C_TABLAS IS
         SELECT *
         FROM ALL_TABLES
          WHERE OWNER = Pv_Owner
          AND TABLE_NAME LIKE Pv_Aplicacion||'%'
          ORDER BY TABLE_NAME;

  CURSOR C_CONSTRAINTS IS
         SELECT *
         FROM ALL_CONSTRAINTS
         WHERE owner = Pv_Owner
         AND TABLE_NAME LIKE Pv_Aplicacion||'_%'
         AND GENERATED = 'USER NAME'
         AND CONSTRAINT_NAME LIKE '%$N$%'
         ORDER BY CONSTRAINT_NAME;

  CURSOR C_CONSTRAINT_TAB (Cv_TableName VARCHAR2) IS
         SELECT *
         FROM ALL_CONSTRAINTS
         WHERE owner = Pv_Owner
         AND TABLE_NAME = Cv_TableName
         AND GENERATED = 'USER NAME'
         AND CONSTRAINT_NAME LIKE '%$N$%'
         ORDER BY CONSTRAINT_NAME;

  CURSOR C_COUNT_CONSTRAINT_TAB (Cv_TableName VARCHAR2) IS
         SELECT COUNT(*)
         FROM ALL_CONSTRAINTS
         WHERE owner = Pv_Owner
         AND TABLE_NAME = Cv_TableName
         AND GENERATED = 'USER NAME'
         AND CONSTRAINT_NAME LIKE '%$N$%'
         ORDER BY CONSTRAINT_NAME;

  CURSOR C_CONSTRAINT_COLUMNS(Cv_NombreConstraint VARCHAR2) IS
         SELECT COLUMN_NAME FROM ALL_CONS_COLUMNS
         WHERE CONSTRAINT_NAME = Cv_NombreConstraint
         AND OWNER = Pv_Owner
         ORDER BY POSITION;

  Lv_ConstName VARCHAR2(30);
  Ln_CantCons  NUMBER;
  Ln_Cont NUMBER := 0;
  Lv_ColName VARCHAR2(30);
BEGIN

  DBMS_OUTPUT.PUT_LINE ('/***************************************************');
  DBMS_OUTPUT.PUT_LINE ('  SCRIPT DEL MODULO DE  '||Pv_Aplicacion);
  DBMS_OUTPUT.PUT_LINE ('                                                 ');
  DBMS_OUTPUT.PUT_LINE ('  AUTOR: '||nvl(Pv_Author,'Lsi. Yajaira Ma. Barragan Arteaga'));
  DBMS_OUTPUT.PUT_LINE ('  FECHA: '|| REPLACE(TO_CHAR(SYSDATE,'DD-MONTH-YYYY'),' ',''));
  DBMS_OUTPUT.PUT_LINE ('***************************************************/');
  DBMS_OUTPUT.PUT_LINE (' ');
  DBMS_OUTPUT.PUT_LINE ('-- CREACION DE NOT NULLS');
  DBMS_OUTPUT.PUT_LINE (' ');
  DBMS_OUTPUT.PUT_LINE ('SPOOL ../'||Pv_Aplicacion||'_NN_001.log');
  DBMS_OUTPUT.PUT_LINE (' ');

  FOR REG_CON IN C_CONSTRAINTS LOOP

      DBMS_OUTPUT.PUT_LINE ('ALTER TABLE '||REG_CON.TABLE_NAME);
      DBMS_OUTPUT.PUT_LINE ('DROP  CONSTRAINT ' ||REG_CON.CONSTRAINT_NAME ||';');
      DBMS_OUTPUT.PUT_LINE (' ');
  END LOOP;

  FOR REG_TAB IN C_TABLAS LOOP
      DBMS_OUTPUT.PUT_LINE ('PROMPT CREANDO NN '||REG_TAB.TABLE_NAME);
      DBMS_OUTPUT.PUT_LINE ('ALTER TABLE '||REG_TAB.TABLE_NAME||' MODIFY(');

      OPEN C_COUNT_CONSTRAINT_TAB(REG_TAB.TABLE_NAME);
      FETCH C_COUNT_CONSTRAINT_TAB INTO Ln_CantCons;
      CLOSE C_COUNT_CONSTRAINT_TAB;

      Ln_Cont := 1;
      FOR REG_CON IN C_CONSTRAINT_TAB (REG_TAB.TABLE_NAME) LOOP
          Lv_ConstName := REG_CON.CONSTRAINT_NAME;
          IF(LENGTH(Lv_ConstName)<30)THEN
             FOR I IN LENGTH(Lv_ConstName) .. 29 LOOP
                Lv_ConstName := Lv_ConstName || ' ';
             END LOOP;
          END IF;
          OPEN C_CONSTRAINT_COLUMNS(REG_CON.CONSTRAINT_NAME);
          FETCH C_CONSTRAINT_COLUMNS INTO Lv_ColName;
          CLOSE C_CONSTRAINT_COLUMNS;
          IF(LENGTH(Lv_ColName)<30)THEN
             FOR I IN LENGTH(Lv_ColName) .. 29 LOOP
                Lv_ColName := Lv_ColName || ' ';
             END LOOP;
          END IF;
          DBMS_OUTPUT.PUT_LINE ('            '||Lv_ColName||' CONSTRAINT '||Lv_ConstName ||' NOT NULL' || CASE WHEN Ln_Cont = Ln_CantCons THEN '' ELSE ',' END);
          Ln_Cont := Ln_Cont +1;
      END LOOP;
      DBMS_OUTPUT.PUT_LINE (')');
      DBMS_OUTPUT.PUT_LINE ('/');
       DBMS_OUTPUT.PUT_LINE (' ');
  END LOOP;

  DBMS_OUTPUT.PUT_LINE ('SPOOL OFF');
EXCEPTION
         WHEN OTHERS THEN
              Pv_MsgError := 'DAF_K_DESARROLLO_RAPIDO->DAF_P_GENERA_SCRIPT_PK ('||SQLCODE ||') ' ||SQLERRM;
END DAF_P_GENERA_SCRIPT_NN;
--
PROCEDURE DAF_P_GENERA_SCRIPT_SY (Pv_Owner VARCHAR2,
                                    Pv_Aplicacion VARCHAR2,
                                    Pv_Author VARCHAR2,
                                    Pv_MsgError OUT VARCHAR2) IS
CURSOR C_SYNONYM IS
  SELECT 'DROP PUBLIC SYNONYM '|| OBJECT_NAME || ' CASCADE CONSTRAINTS'  linea
  FROM ALL_OBJECTS
  WHERE OWNER = Pv_Owner
  AND OBJECT_NAME LIKE Pv_Aplicacion||'_%'
  AND OBJECT_TYPE IN ('TABLE', 'SEQUENCE')
  ORDER BY OBJECT_TYPE DESC,OBJECT_NAME;

CURSOR C_SYNONYM2 IS
  SELECT 'CREATE OR REPLACE PUBLIC SYNONYM '|| OBJECT_NAME || ' FOR '||Pv_Owner ||'.'  || OBJECT_NAME  linea
  FROM ALL_OBJECTS
  WHERE OWNER = Pv_Owner
  AND OBJECT_NAME LIKE Pv_Aplicacion||'_%'
  AND OBJECT_TYPE IN ('TABLE', 'SEQUENCE')
  ORDER BY OBJECT_TYPE DESC,OBJECT_NAME;
BEGIN
  DBMS_OUTPUT.PUT_LINE ('/***************************************************');
  DBMS_OUTPUT.PUT_LINE ('  SCRIPT DEL MODULO DE  '||Pv_Aplicacion);
  DBMS_OUTPUT.PUT_LINE ('                                                 ');
  DBMS_OUTPUT.PUT_LINE ('  AUTOR: '||nvl(Pv_Author,'Lsi. Yajaira Ma. Barragan Arteaga'));
  DBMS_OUTPUT.PUT_LINE ('  FECHA: '|| REPLACE(TO_CHAR(SYSDATE,'DD-MONTH-YYYY'),' ',''));
  DBMS_OUTPUT.PUT_LINE ('***************************************************/');
  DBMS_OUTPUT.PUT_LINE (' ');
  DBMS_OUTPUT.PUT_LINE ('-- CREACION DE SINONIMOS');
  DBMS_OUTPUT.PUT_LINE (' ');
  DBMS_OUTPUT.PUT_LINE ('SPOOL ../'||Pv_Aplicacion||'_SY_001.log');
  DBMS_OUTPUT.PUT_LINE (' ');

  FOR REG_SYN IN C_SYNONYM LOOP
      DBMS_OUTPUT.PUT_LINE (REG_SYN.Linea||';');
  END LOOP;
  DBMS_OUTPUT.PUT_LINE ('');
  DBMS_OUTPUT.PUT_LINE ('--CREACION DE SINONIMOS');
  DBMS_OUTPUT.PUT_LINE ('');
  FOR REG_SYN IN C_SYNONYM2 LOOP
      DBMS_OUTPUT.PUT_LINE (REG_SYN.Linea);
      DBMS_OUTPUT.PUT_LINE ('/');
      DBMS_OUTPUT.PUT_LINE ('');
  END LOOP;

  DBMS_OUTPUT.PUT_LINE ('SPOOL OFF');
EXCEPTION
         WHEN OTHERS THEN
              Pv_MsgError := 'DAF_K_DESARROLLO_RAPIDO->DAF_P_GENERA_SCRIPT_SY ('||SQLCODE ||') ' ||SQLERRM;
END DAF_P_GENERA_SCRIPT_SY;
--
PROCEDURE DAF_P_GENERA_SCRIPT_GR (Pv_Owner VARCHAR2,
                                  Pv_Aplicacion VARCHAR2,
                                  Pv_Author VARCHAR2,
                                  Pv_MsgError OUT VARCHAR2) IS
CURSOR C_GRANT1 IS
  SELECT 'GRANT SELECT, INSERT, UPDATE, DELETE ON ' || Pv_Owner||'.'|| OBJECT_NAME || ' TO PUBLIC' ||'; ' GRANTED
  FROM ALL_OBJECTS
  WHERE OWNER = Pv_Owner
  AND OBJECT_NAME LIKE Pv_Aplicacion||'_%'
  AND OBJECT_TYPE IN ('TABLE')
  ORDER BY OBJECT_NAME;

CURSOR C_GRANT2 IS
  SELECT 'GRANT SELECT ON '|| Pv_Owner||'.' || OBJECT_NAME || ' TO PUBLIC' ||'; ' GRANTED
  FROM ALL_OBJECTS
  WHERE OWNER = Pv_Owner
  AND OBJECT_NAME LIKE Pv_Aplicacion||'_%'
  AND OBJECT_TYPE IN ('SEQUENCE')
  ORDER BY OBJECT_NAME;


BEGIN
  DBMS_OUTPUT.PUT_LINE ('/***************************************************');
  DBMS_OUTPUT.PUT_LINE ('  SCRIPT DEL MODULO DE  '||Pv_Aplicacion);
  DBMS_OUTPUT.PUT_LINE ('                                                 ');
  DBMS_OUTPUT.PUT_LINE ('  AUTOR: '||nvl(Pv_Author,'Lsi. Yajaira Ma. Barragan Arteaga'));
  DBMS_OUTPUT.PUT_LINE ('  FECHA: '|| REPLACE(TO_CHAR(SYSDATE,'DD-MONTH-YYYY'),' ',''));
  DBMS_OUTPUT.PUT_LINE ('***************************************************/');
  DBMS_OUTPUT.PUT_LINE (' ');
  DBMS_OUTPUT.PUT_LINE ('-- CREACION DE PERMISOS');
  DBMS_OUTPUT.PUT_LINE (' ');
  DBMS_OUTPUT.PUT_LINE ('SPOOL ../'||Pv_Aplicacion||'_GR_001.log');
  DBMS_OUTPUT.PUT_LINE (' ');
  DBMS_OUTPUT.PUT_LINE ('--TABLAS');
  DBMS_OUTPUT.PUT_LINE ('');
  FOR REG_GRA IN C_GRANT1 LOOP
      DBMS_OUTPUT.PUT_LINE (REG_GRA.GRANTED);
      DBMS_OUTPUT.PUT_LINE ('');
  END LOOP;
  DBMS_OUTPUT.PUT_LINE ('');
  DBMS_OUTPUT.PUT_LINE ('--SECUENCIAS');
  DBMS_OUTPUT.PUT_LINE ('');
  FOR REG_GRA IN C_GRANT2 LOOP
      DBMS_OUTPUT.PUT_LINE (REG_GRA.GRANTED);
      DBMS_OUTPUT.PUT_LINE ('');
  END LOOP;

  DBMS_OUTPUT.PUT_LINE ('SPOOL OFF');
EXCEPTION
         WHEN OTHERS THEN
              Pv_MsgError := 'DAF_K_DESARROLLO_RAPIDO->DAF_P_GENERA_SCRIPT_GR ('||SQLCODE ||') ' ||SQLERRM;
END DAF_P_GENERA_SCRIPT_GR;
--
PROCEDURE DAF_P_GENERA_SCRIPT_CK (Pv_Owner        VARCHAR2,
                                  Pv_Aplicacion   VARCHAR2,
                                  Pv_Author       VARCHAR2,
                                  Pv_MsgError OUT VARCHAR2) IS

  CURSOR C_CONSTRAINTS IS
         SELECT *
         FROM ALL_CONSTRAINTS
         WHERE owner = Pv_Owner
         AND TABLE_NAME LIKE Pv_Aplicacion||'_%'
         AND GENERATED = 'USER NAME'
         AND CONSTRAINT_NAME LIKE '%$C$%'
         ORDER BY CONSTRAINT_NAME;

BEGIN

  DBMS_OUTPUT.PUT_LINE ('/***************************************************');
  DBMS_OUTPUT.PUT_LINE ('  SCRIPT DEL MODULO DE  '||Pv_Aplicacion);
  DBMS_OUTPUT.PUT_LINE ('                                                 ');
  DBMS_OUTPUT.PUT_LINE ('  AUTOR: '||nvl(Pv_Author,'Lsi. Yajaira Ma. Barragan Arteaga'));
  DBMS_OUTPUT.PUT_LINE ('  FECHA: '|| REPLACE(TO_CHAR(SYSDATE,'DD-MONTH-YYYY'),' ',''));
  DBMS_OUTPUT.PUT_LINE ('***************************************************/');
  DBMS_OUTPUT.PUT_LINE (' ');
  DBMS_OUTPUT.PUT_LINE ('-- CREACION DE CHECKS');
  DBMS_OUTPUT.PUT_LINE (' ');
  DBMS_OUTPUT.PUT_LINE ('SPOOL ../'||Pv_Aplicacion||'_CK_001.log');
  DBMS_OUTPUT.PUT_LINE (' ');

  FOR REG_CON IN C_CONSTRAINTS LOOP

      DBMS_OUTPUT.PUT_LINE ('ALTER TABLE '||REG_CON.TABLE_NAME);
      DBMS_OUTPUT.PUT_LINE ('DROP  CONSTRAINT ' ||REG_CON.CONSTRAINT_NAME ||';');
      DBMS_OUTPUT.PUT_LINE (' ');
  END LOOP;

  FOR REG_CON IN C_CONSTRAINTS LOOP
      DBMS_OUTPUT.PUT_LINE ('ALTER TABLE '||REG_CON.TABLE_NAME);
      DBMS_OUTPUT.PUT_LINE ('ADD  CONSTRAINT ' ||REG_CON.CONSTRAINT_NAME ||' CHECK(' || REG_CON.SEARCH_CONDITION ||')');
      DBMS_OUTPUT.PUT_LINE ('/ ');
      DBMS_OUTPUT.PUT_LINE (' ');
  END LOOP;

  DBMS_OUTPUT.PUT_LINE ('SPOOL OFF');
EXCEPTION
         WHEN OTHERS THEN
              Pv_MsgError := 'DAF_K_DESARROLLO_RAPIDO->DAF_P_GENERA_SCRIPT_CK ('||SQLCODE ||') ' ||SQLERRM;
END DAF_P_GENERA_SCRIPT_CK;
--
PROCEDURE DAF_P_GENERA_SCRIPT_SQ (Pv_Owner        VARCHAR2,
                                  Pv_Aplicacion   VARCHAR2,
                                  Pv_Author       VARCHAR2,
                                  Pv_MsgError OUT VARCHAR2)IS
CURSOR C_SECUENCIAS IS
  SELECT OBJECT_NAME
  FROM ALL_OBJECTS
  WHERE OWNER = Pv_Owner
  AND OBJECT_NAME LIKE Pv_Aplicacion||'_%'
  AND OBJECT_TYPE IN ('SEQUENCE')
  ORDER BY OBJECT_NAME;
BEGIN

  DBMS_OUTPUT.PUT_LINE ('/***************************************************');
  DBMS_OUTPUT.PUT_LINE ('  SCRIPT DEL MODULO DE  '||Pv_Aplicacion);
  DBMS_OUTPUT.PUT_LINE ('                                                 ');
  DBMS_OUTPUT.PUT_LINE ('  AUTOR: '||nvl(Pv_Author,'Lsi. Yajaira Ma. Barragan Arteaga'));
  DBMS_OUTPUT.PUT_LINE ('  FECHA: '|| REPLACE(TO_CHAR(SYSDATE,'DD-MONTH-YYYY'),' ',''));
  DBMS_OUTPUT.PUT_LINE ('***************************************************/');
  DBMS_OUTPUT.PUT_LINE (' ');
  DBMS_OUTPUT.PUT_LINE ('-- CREACION DE SECUENCIAS');
  DBMS_OUTPUT.PUT_LINE (' ');
  DBMS_OUTPUT.PUT_LINE ('SPOOL ../'||Pv_Aplicacion||'_SQ_001.log');
  DBMS_OUTPUT.PUT_LINE (' ');
  FOR REG_SEQ IN C_SECUENCIAS LOOP
      DBMS_OUTPUT.PUT_LINE ('CREATE SEQUENCE '||REG_SEQ.OBJECT_NAME);
      DBMS_OUTPUT.PUT_LINE ('MINVALUE 1');
      DBMS_OUTPUT.PUT_LINE ('MAXVALUE 9999999');
      DBMS_OUTPUT.PUT_LINE ('START WITH 1');
      DBMS_OUTPUT.PUT_LINE ('INCREMENT by 1');
      DBMS_OUTPUT.PUT_LINE ('NOCACHE');
      DBMS_OUTPUT.PUT_LINE ('ORDER');
      DBMS_OUTPUT.PUT_LINE ('/');
      DBMS_OUTPUT.PUT_LINE (' ');
  END LOOP;
  DBMS_OUTPUT.PUT_LINE ('SPOOL OFF');
EXCEPTION
         WHEN OTHERS THEN
              Pv_MsgError := 'DAF_K_DESARROLLO_RAPIDO->DAF_P_GENERA_SCRIPT_CK ('||SQLCODE ||') ' ||SQLERRM;
END DAF_P_GENERA_SCRIPT_SQ;
END DAF_K_DESARROLLO_RAPIDO;
/
