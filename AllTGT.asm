title "Myanmar Flag"

ORG 100H


;   MYANMAR FLAG IS DRAWN HERE IN THE (320 X 200) PIXELS SCREEN.
;   IT IS DIVIDED INTO 4 PARTS - THE BACKGROUND AND 3 PARTS OF THE STAR.
;
;   THE STAR IS DIVIDED INTO A TRIANGLE, A TRAPEZIUM AND THEN TWO REFLECTING TRIANGLES.
;
;    FOLLOWING DATA FOR EACH PART HAS BEEN EXPLAINED IN DATA SEGMENT.
;
;    PROCEDURE IDEA HAS BEEN EXPLAINED BEFORE DECLARING THE VARIABALES IN DATA SEGMENT
;
;
;




.MODEL SMALL 

.STACK 100H

    ;    
    ;
    ;     BACKGROUND DONE HERE.
    ;
    ;     THREE RECTANGLES:- YELLOW, GREEN AND RED;
    ;
    ;
    ;     MACRO REC_FILL TAKES THE INITIAL AND FINAL Y CO-ORDINATES AND THE COLOR YOU WANT TO FILL A RECTANGLE WITH AS PARAMETERS.
    ;     THE SIZE OF THE RECTANGLE IS FIXED FOR NOW. IT IS 1/3RD OF THE SIZE OF THE (320 X 200) SCREEN.
    
    ;     Y CO-ORDINATES ARE ONLY TAKEN BECAUSE THE X CO-ORDINATE IS ANYWAY THE WHOLE LINE GOING FROM LEFT TO RIGHT
    ;     OF THE SCREEN. ONLY THE Y CO-ORDINATE CONTROLS THE SEPARATE RECTANGLES (AS IN MYANMAR FLAG)
    
    ;
    ;
    ;
    ;
    
    
    
    REC_FILL  MACRO REC_Y1, REC_Y2, REC_COLOR 
           LOCAL VERTICALBOUND, HORIZONTALBOUND, LINECOLORED, DONEFILL
            
            MOV DX,REC_Y1
        
            VERTICALBOUND:             ;CHECKING IF LAST LINE OF THE RECTANGLE GIVEN IS COLORED
        
        CMP DX,REC_Y2
        JG DONEFILL
        
        MOV CX, 0H     
        
                HORIZONTALBOUND:
                
                CMP CX, 320D        
                JG LINECOLORED
                
                
        
                 MOV AL,REC_COLOR       ;THE LINESELECTED IS NOW COLORED FROM LEFT TO RIGHT
                 MOV AH, 0CH
                 INT 10H
                 
                 INC CX
                 JMP HORIZONTALBOUND 
                 
        LINECOLORED:
        
       
        INC DX   ;NEXT HORIZONTAL LINE NOW
        
        JMP VERTICALBOUND
        
        
            DONEFILL:      
        
        
        ENDM 
    

.DATA


REC_Y1P DW 0H    
REC_Y2P DW 67D    ;1/3RD OF THE SCREEN = THE RECTANGLE'S BREADTH

REC_COLORP DB 14D        ;YELLOW 


 ;;;;;;;;;;;;;;RECTANGLE DATA ENDS ABOVE;;;;;;;;;;;;;;;;;;;;;;;;;;;  
 
 
 

; 
;   IDEA IS TO DRAW VERTICAL LINES FROM PEAK TO BASE FROM MIDLINE, ONCE IN THE LEFT DIRECTION AND AGAIN IN THE RIGHT DIRECTION
;
;
;    (X1,Y1) IS THE PEAK OF THE TRIANGLE, (X2, Y2) AND (X3,Y3) ARE THE CO-ORDINATES OF THE BASE.
;
;
;
;

TRI_X1 DW 155D         ;FIRST TRIANGLE, YELLOW TO GREEN. THIS IS THE PEAK
TRI_Y1 DW 40D
TRI_X2 DW 115D         ;BASE LINE'S INITIAL POINT      (w.r.t X CO-ORDINATES)
TRI_Y2 DW 80D
TRI_X3 DW 195D         ;BASE LINE'S FINAL POINT
TRI_Y3 DW 80D 
                         ;INITIALLY THE LINE FROM PEAK TO BASE
LINEST_X DW 155D         ;VARIABLES FOR LOOP MAINLY. SINCE INITIAL POINT OF LINE VARIES
LINEST_Y DW 40D
          



 ;;;;;;;;;;;;;;FIRST TRIANGLE DATA ENDS ABOVE;;;;;;;;;;;;;;;;;;;;;;;;;;;  
 






;   A TRAPEZOID IS CONSIDERED HERE. (X1,Y1) , (X2,Y2) AND (X3,Y3), (X4,Y4) ARE THE TWO PARALLEL LINES
;   Y1 = Y2 AND Y3 = Y4.
;   X SHIFTS RIGHT (INCREASES) FOR EACH LINE BUT LIMIT DECREASES (SINCEWIDTH IS DECREASING FROM TOP TO BOTTOM FOR THE TRAPEZOID)
;   Y INCREASES UNIFORMLY FROM Y1 TO Y3 AND STOPS WHEN Y3 IS COMPLETE
;
;
;
;
;
;

MTRI_X1 DW 90D
MTRI_Y1 DW 80D
MTRI_X2 DW 220D
                ;Y1 = Y2

MTRI_Y3 DW 120D

                ;Y3 = Y4 
                
                ;X CO-ORDINATES CALCULATED FROM FIRST LARGEST LINE SO NOT NEEDED

TRLINE_X DW 90D          ;VARIABLES FOR NESTED LOOP. SINCE LINE'S BREADTH DECREASE BUT LINE'S Y CO-ORDINATE INCREASES


TRLINE_ENDX DW  220D     ;FIRST LINE'S INITIAL WIDTH 

 

;;;;;;;;;;;;;;TRAPEZIUM DATA ENDS ABOVE;;;;;;;;;;;;;;;;;;;;;;;;;;;  
 



;IDEA IS TO DRAW HORIZONTAL LINES FROM PEAK TO BASE 
 ;
 ;
 ;
 ;
 ;
 ;

TRI_BASEX1 DW 130D       ;BASE LINE'S CO OR-DINATES
TRI_BASEY1 DW 110D

TRI_BASEX2 DW 184D ;BASE HAS SAME Y CO-ORDINATES

TRI_PEAKX DW 80D         ;PEAK OF THE TRIANGLE, FROM HERE WE WILL DRAW THE LINES UPTO BASELINE
TRI_PEAKY DW 160D

LINEST_X1  DW 80D       ;INITIALLY THE PEAK AND NO LINE AT THE PEAK, ONLY 1 PIXEL         ;X1 TO AVOID SAME NAME
LINE_WIDTH DW 80D    


TRI_PEAKX1 DW 230D       ;THE PEAK OF THE SECOND TRIANGLE, REFLECTION OF THE FIRST. Y CO-ORDINATE SAME



;
;
;   CODE SEGMENT !
;
;
;
;

.CODE 


        MAIN PROC
            
            MOV DX, @DATA
            MOV DS, DX
        
        
        ;al = 13H -  320 x 200 pixels, ah = 0 -  blank screen
        ;int 10H  = setting video mode
        
        
        MOV AL, 13h
        MOV AH, 0H
        INT 10h 
            
            
            
            CALL BACKGROUND
            
            CALL FIRST_TRIANGLE
            
            CALL TRAPEZIUM 
            
           CALL LAST2TRIANGLES
            
            
            
            EXITTODOS:
            
            ;WAIT FOR KEYPRESSS
            
            MOV AH, 1
            INT 21H  
            
            MOV AX, 0H
            INT 10H 
            
            MOV AH, 4CH
            INT 21H        
            
            ENDP MAIN

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
   BACKGROUND PROC
        
      
        
       
        ;FIRST TIME DATA IS SET WHEN DECLARED (FOR YELLOW RECTANGLE)
        
                
        REC_FILL REC_Y1P, REC_Y2P, REC_COLORP            
                                                   
 
        ;ADJUSTMENTS FOR GREEN. Y- COORDINATES CHANGED AND COLOR CODE.
        
        MOV REC_Y1P, 67D
        MOV REC_Y2P, 133D
        MOV REC_COLORP, 02H
        
         
        REC_FILL REC_Y1P,REC_Y2P, REC_COLORP     
        
        ;SIMILARLY FOR RED
        
                  
        MOV REC_Y1P, 133D
        MOV REC_Y2P, 201D
        MOV REC_COLORP, 04H                                               
       
        REC_FILL REC_Y1P,REC_Y2P, REC_COLORP 
      
         
        
        
        
       RET
    BACKGROUND ENDP  
   
    ;IDEA IS TO DRAW VERTICAL LINES FROM PEAK TO BASE. EVERY TIME IT WILL STOP WHEN IT TOUCHES THE BASE (Y CO-ORDINATES MATCH!)
        ;LEFT-HALF AND RIGHT-HALF ARE DIFFERENT FOR THE CHANGING PATTERN OF X CO-ORDINATE
        ;IN RIGHT-HALF, X INCREASES AND OPPOSITE IN LEFT-HALF
        ;Y CO-ORDINATES FOLLOW THE SAME ORDER FOR BOTH HALVES
        ;CO-ORDINATES ARE CONTROLLED BY VARIABLES LINEST_X AND LINEST_Y (LINESTART X,Y)
        ;
        ;
        
         FIRST_TRIANGLE PROC 
            
        
                    
        
        CALL RIGHTHALFCOLOR

       ;------------------------------------------------------------------------------------------------;
       
       CALL LEFTHALFCOLOR                  ;;;;;;;;;;;;;;PROBLEM!!!!!!!;;;;;;;;;;;;;;;;;;;;;;;;;
        
       ;--------------------------------------------------------------------------------------------------; 
        
        RET
        
        
        FIRST_TRIANGLE   ENDP   


             
        
        
        LEFTHALFCOLOR PROC 
            
            ;LEFT-HALF OF THE TRIANGLE 
        
       
        
        MOV CX, TRI_X1
        MOV DX, TRI_Y1 
        
        ;RESETTING THE FIRST LINE AGAIN, FROM PEAK TO BASE
        
        MOV BX, TRI_X1
        MOV LINEST_X, BX
        
        MOV BX, TRI_Y1
        MOV LINEST_Y, BX
        
        
        PCKLN2:
        
        CMP CX,TRI_X2
        JLE HLFDN2
        
               
        MOV BX, LINEST_Y     ;KEEPING Y CO-ORDINATE OF NEXT VERTICAL LINE FROM HERE, SINCE DX GETS USED.
        INC BX
        INC BX
        MOV LINEST_Y, BX
        
            VRTCLLN2:
                 
                 CMP DX, TRI_Y2    ;Y2,Y3 SAME HERE, DOESN'T MATTER WHAT WE COMPARE WITH
                 JGE ENDLN2
                 
                   
                 MOV AL, 15D      ;THE LINE SELECTED IS NOW COLORED FROM TOP TO BOTTOM   ;WHITE COLOR FIXED
                 MOV AH, 0CH
                 INT 10H
                 
                 INC DX
                 
                 JMP VRTCLLN2
        
        
                ENDLN2:
                
                DEC CX              ;GOING LEFT
                MOV DX, LINEST_Y    ;NEXT LINE'S Y WAS SET BEFORE. TAKING THAT DIRECTLY
                
                JMP PCKLN2       
                
       
       
       HLFDN2:
       
  
       RET 
       
       
       LEFTHALFCOLOR ENDP
        
        
        
       
       
       
       RIGHTHALFCOLOR PROC 
            
            ;RIGHT-HALF OF THE TRIANGLE 
      
        
        
        MOV CX, TRI_X1
        MOV DX, TRI_Y1 
        
        ;RESETTING THE FIRST LINE AGAIN, FROM PEAK TO BASE
        
        MOV BX, TRI_X1
        MOV LINEST_X, BX
        
        MOV BX, TRI_Y1
        MOV LINEST_Y, BX
        
        
        PICKLINE:
        
        CMP CX,TRI_X3
        JGE HALFDONE
        
        
        
        
        MOV BX, LINEST_Y     ;KEEPING Y CO-ORDINATE OF NEXT VERTICAL LINE FROM HERE, SINCE DX GETS USED.
        INC BX
        INC BX               ;TO MAKE THE TRIANGLE SHARPER, INC TWICE
        MOV LINEST_Y, BX
        
            VERTICALLINE:
                 
                 CMP DX, TRI_Y3    ;Y2,Y3 SAME HERE, DOESN'T MATTER WHAT WE COMPARE WITH
                 JGE ENDLINE
                 
                   
                 MOV AL, 15D      ;THE LINE SELECTED IS NOW COLORED FROM TOP TO BOTTOM
                 MOV AH, 0CH
                 INT 10H
                 
                 INC DX
                 
                 JMP VERTICALLINE
        
        
                ENDLINE:
                
                INC CX              ;GOING RIGHT
                MOV DX, LINEST_Y    ;NEXT LINE'S Y WAS SET BEFORE. TAKING THAT DIRECTLY
                
                JMP PICKLINE       
                
       HALFDONE:
            
            
            RET
            
            
        RIGHTHALFCOLOR  ENDP 
        

        
        
        
        
        
        TRAPEZIUM PROC NEAR
            
     
        
        MOV CX, MTRI_X1              ;TAKING THE FIRST LARGEST LINE'S INITIAL POINT
        MOV DX, MTRI_Y1
        
              VERTICALLMT:            ;LENGTH OF TRAPEZOID
                  
                  
                  CMP DX, MTRI_Y3      ;CHECKING IF WE HAVE FILLED THE LAST LINE OF TRAPEZOID
                  JG TRDONE 
                  
                   MOV CX, TRLINE_X    ;X CHANGES FOR EACH NEXT LINE, SO TAKING THAT HERE     
                           
                          
                           HORIZONTALLMT:                             
                            
                            CMP CX, TRLINE_ENDX         ;WIDTH OF THE LINE CHECKED HERE
                            JG TRLINEFILLED
                            
                             MOV AL, 15D
                             MOV AH, 0CH
                             INT 10H
                             
                             INC CX
                             JMP HORIZONTALLMT
                           
                  
                  TRLINEFILLED:
                  
                                   
                  MOV BX, TRLINE_X     ;NEXT LINE , X CO-ORDINATE SHIFTS RIGHT
                  INC BX
                  MOV TRLINE_X,BX
                  
                  MOV BX, TRLINE_ENDX     ;WIDTH DECREASES FOR EACH NEXT LINE IN TRAPEZOID
                  DEC BX
                  MOV TRLINE_ENDX, BX
                  
                  INC DX                ;MOVING TO NEXT LINE'S Y CO-ORDINATE FROM HERE
                  
                  JMP VERTICALLMT
                  
                  TRDONE:            ;TRAPEZOID HAS BEEN FILLED
                       
                       
                       ;TRACING FOR OTHER PURPOSE
                       ;MOV BX, CX               ;KEEPING INITIAL X CO-ORDINATES
                       ;MOV CX, TRLINE_ENDX      ;KEEPING FINAL X CO-ORDINATES
            
                       RET
            
            
            
        ENDP TRAPEZIUM 
        
        
        
        LAST2TRIANGLES PROC
            
        
        
        
        MOV CX, TRI_PEAKX
        MOV DX, TRI_PEAKY
        
            VERTICALBND:             ;VERTICAL BOUND CHECK HERE, IF BASELINE REACHED
                
                CMP DX, TRI_BASEY1
                JL TRIDONE
                
                MOV CX, LINEST_X1
                
                HORIZONTALBND:
                
                   CMP CX, LINE_WIDTH
                   JG FILLED
                   
                   MOV AL, 15D
                   MOV AH,0CH
                   INT 10H
                   
                   INC CX
                   JMP HORIZONTALBND
                
                FILLED:
                
                MOV BX, LINEST_X1     ;X CO-ORDINATE SHIFTS RIGHT FOR EACH NEXT LINE SO THAT IS ADJUSTED HERE
                INC BX
                MOV LINEST_X1, BX      
                                     
                                     
                                     ;WIDTH INCREASES FOR EACH LINE 
                MOV BX, LINE_WIDTH
                INC BX
                INC BX
                
                MOV LINE_WIDTH, BX                     
                
                DEC DX               ;GOING BOTTOM TO TOP 
                
                
                JMP VERTICALBND
                
                
                TRIDONE:
                
                
                ;SECOND TRIANGLE, MIRROR REFLECTION 
                
                MOV BX, TRI_PEAKX1         ;AS DONE PREVIOUSLY, PEAK IS THE FIRST LINE WITH NO WIDTH
                MOV LINEST_X1, BX
                
                MOV LINE_WIDTH, BX
                
                ;NOW THE BASE-LINE'S POINTS WILL BE INTERCHANGED 
                
                
        MOV CX, TRI_PEAKX1
        MOV DX, TRI_PEAKY
        
            VERTICALBND2:             ;VERTICAL BOUND CHECK HERE, IF BASELINE REACHED
                
                CMP DX, TRI_BASEY1
                JL TRIDONE2
                
                MOV CX, LINEST_X1
                
                HORIZONTALBND2:
                
                   CMP CX, LINE_WIDTH
                   JL FILLED2
                   
                   MOV AL, 15D
                   MOV AH,0CH
                   INT 10H
                                           ;COLORS FROM RIGHT TO LEFT
                   DEC CX                  
                   JMP HORIZONTALBND2
                
                FILLED2:
                
                MOV BX, LINEST_X1     ;X CO-ORDINATE SHIFTS LEFT FOR EACH NEXT LINE SO THAT IS ADJUSTED HERE
                DEC BX
                MOV LINEST_X1, BX      
                                     
                                     ;WIDTH INCREASES FOR EACH LINE BUT CO-ORDINATE SHIFTS LEFT
                MOV BX, LINE_WIDTH
                DEC BX
                DEC BX 
                
                MOV LINE_WIDTH, BX                     
                
                DEC DX               ;GOING BOTTOM TO TOP 
                
                
                JMP VERTICALBND2
                
                
                TRIDONE2:
        
            
                     RET
                     
            
            ENDP LAST2TRIANGLES
     
    
    
      END
  