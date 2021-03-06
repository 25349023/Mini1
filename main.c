#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "myLex.h"
/*
Something like Python
>> y = 2
>> z = 2
>> x = 3*y + 4/(2*z)

*/

/*
the only type: integer
everything is an expression
  statement   := END | expr END

  expr        := term expr_tail
  expr_tail   := BITOR term expr_tail | NIL

  term        := term2 term_tail
  term_tail   := BITXOR term2 term_tail | NIL

  term2       := term3 term2_tail
  term2_tail  := BITAND term3 term2_tail | NIL

  term3       := term4 term3_tail
  term3_tail  := ADDSUB term4 term3_tail | NIL

  term4       := factor term4_tail
  term4_tail  := MULDIV factor term4_tail | NIL

  factor      := INT | ADDSUB INT | ADDSUB ID | ID ASSIGN expr | ID | LPAREN expr RPAREN
*/

#define TBLSIZE 65535
typedef struct {
    char name[MAXLEN];
    int val;
} Symbol;
Symbol table[TBLSIZE];
int sbcount = 0;

typedef struct _Node {
    char lexeme[MAXLEN];
    TokenSet token;
    int val, sign;
    struct _Node *left, *right;
} BTNode;

void statement(void);
BTNode* expr(void);
BTNode* term(void);
BTNode* term2(void);
BTNode* term3(void);
BTNode* term4(void);
BTNode* factor(void);
int getval(void);
int setval(char*, int);

typedef enum { MISPAREN, NOTNUMID, NOTFOUND, RUNOUT, NAN, SYNERR, FINISH } ErrorType;
void error(ErrorType errorNum);

/// ::
/// my code start

typedef struct {
    int value;
    char type;  // 'l' for left exp, 'r' for right exp, 'a' for left assign, 'm' for root
    int storeInReg;
} RetInfo;


char regContent[8][MAXLEN];
int regInUse[8] = {0};
char regName[8][3] = {"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7"};
char memory[100][MAXLEN];
int currMaxMemIndex = 2;

int usedMemInCurStat = 0;
int volatileMem[100] = {0};

int idNumCount = 0;
int reduceCntStack[2048] = {0}, top = -1;

int assignAppeared = 0;

int findIdleReg(){
    for (int i = 0; i < 8; i++){
        if (!regInUse[i] && strcmp(regContent[i], "###") == 0){
            return i;
        }
    }
    for (int i = 0; i < 8; i++){
        if (!regInUse[i]){
            return i;
        }
    }
    return -1;
}

int findAddrInMem(const char *name){
    for (int i = 0; i <= currMaxMemIndex && i < 100; i++){
        if (strcmp(memory[i], name) == 0){
            return i;
        }
    }
    return -1;
}

int findVarInReg(const char *name){
    for (int i = 0; i < 8; i++){
        if (strcmp(name, regContent[i]) == 0){
            return i;
        }
    }
    return -1;
}


/// ::
/// my code end

/* create a node without any child */
BTNode* makeNode(TokenSet tok, const char *lexe)
{
    BTNode *node = (BTNode*) malloc(sizeof(BTNode));
    strcpy(node->lexeme, lexe);
    node->token= tok;
    node->val = 0;
    node->sign = 0;
    node->left = NULL;
    node->right = NULL;
    // printf("make node lexe: %s\n", lexe);
    return node;
}

/* clean a tree */
void freeTree(BTNode *root)
{
    if (root!=NULL) {
        freeTree(root->left);
        freeTree(root->right);
        free(root);
    }
}

/* print a tree by pre-order */
void printPrefix(BTNode *root)
{
    if (root != NULL) {
        printf("%s ", root->lexeme);
        printPrefix(root->left);
        printPrefix(root->right);
    }
}

/* traverse the syntax tree by pre-order
   and evaluate the underlying expression */
RetInfo evaluateTree(BTNode *root, char mode)
{
    int retval = 0, lv, rv;
    RetInfo finalEvl, li = {.storeInReg = -1}, ri;
    finalEvl.storeInReg = -1;
    finalEvl.type = mode;
    if (root != NULL) {
        switch (root->token) {
            case ID: {
                if (mode == 'm'){
                    int addr = findAddrInMem(root->lexeme) * 4;
                    if (addr < 0){ error(NOTFOUND); }
                }
                else if (mode != 'a'){
                    int addr = findAddrInMem(root->lexeme) * 4;
                    if (addr < 0){ error(NOTFOUND); }
                    int availReg = findIdleReg();
                    int posInReg = findVarInReg(root->lexeme);
                    if (volatileMem[addr / 4]){
                        usedMemInCurStat = 1;
                        if (posInReg == -1 || regInUse[posInReg]){
                            printf("MOV %s [%d]\n", regName[availReg], addr);
                            strcpy(regContent[availReg], root->lexeme);
                        }
                        else {
                            availReg = posInReg;
                        }
                        if (root->sign){
                            regInUse[availReg] = 1;
                            int avl = findIdleReg();
                            printf("MOV %s 0\n", regName[avl]);
                            strcpy(regContent[avl], "###");
                            printf("SUB %s %s\n", regName[avl], regName[availReg]);
                            regInUse[availReg] = 0;
                            availReg = avl;
                        }
                    }
                    else {
                        if (posInReg == -1 || regInUse[posInReg]){
                            printf("MOV %s %d\n", regName[availReg], root->val);
                            strcpy(regContent[availReg], root->lexeme);
                        }
                        else {
                            availReg = posInReg;
                        }
                    }
                    regInUse[availReg] = 1;
                    finalEvl.storeInReg = availReg;
                }
                else {
                    if (findAddrInMem(root->lexeme) < 0){
                        currMaxMemIndex++;
                        strcpy(memory[currMaxMemIndex], root->lexeme);
                    }
                }
                retval = root->val;
                break;
            }
            case INT: {
                if (mode != 'm'){
                    int availReg = findIdleReg();
                    printf("MOV %s %d\n", regName[availReg], root->val);
                    strcpy(regContent[availReg], "###");
                    regInUse[availReg] = 1;
                    finalEvl.storeInReg = availReg;
                }
                retval = root->val;
                break;
            }
            case ASSIGN:
            case BITAND:
            case BITXOR:
            case BITOR:
            case ADDSUB:
            case MULDIV:
                li = evaluateTree(root->left, (char) (root->token == ASSIGN ? 'a' : 'l'));
                ri = evaluateTree(root->right, 'r');
                lv = li.value;
                rv = ri.value;
                if (strcmp(root->lexeme, "+") == 0){
                    printf("ADD %s %s\n", regName[li.storeInReg], regName[ri.storeInReg]);
                    strcpy(regContent[li.storeInReg], "###");
                    retval = lv + rv;
                }
                else if (strcmp(root->lexeme, "-") == 0){
                    printf("SUB %s %s\n", regName[li.storeInReg], regName[ri.storeInReg]);
                    strcpy(regContent[li.storeInReg], "###");
                    retval = lv - rv;
                }
                else if (strcmp(root->lexeme, "*") == 0){
                    printf("MUL %s %s\n", regName[li.storeInReg], regName[ri.storeInReg]);
                    strcpy(regContent[li.storeInReg], "###");
                    retval = lv * rv;
                }
                else if (strcmp(root->lexeme, "/") == 0) {
                    if (rv == 0 && !volatileMem[findAddrInMem(root->right->lexeme)]){
                        error(NAN);
                    }
                    else {
                        printf("DIV %s %s\n", regName[li.storeInReg], regName[ri.storeInReg]);
                        strcpy(regContent[li.storeInReg], "###");
                        if (rv != 0){
                            retval = lv / rv;
                        }
                        else {
                            retval = lv;  // trivial
                        }
                    }
                } else if (strcmp(root->lexeme, "=") == 0){
                    retval = setval(root->left->lexeme, rv);
                    int addr = findAddrInMem(root->left->lexeme) * 4;
                    printf("MOV [%d] %s\n", addr, regName[ri.storeInReg]);
                    if (usedMemInCurStat){
                        volatileMem[addr / 4] = 1;
                    }
                }
                else if (strcmp(root->lexeme, "&") == 0){
                    printf("AND %s %s\n", regName[li.storeInReg], regName[ri.storeInReg]);
                    strcpy(regContent[li.storeInReg], "###");
                    retval = lv & rv;
                }
                else if (strcmp(root->lexeme, "|") == 0){
                    printf("OR %s %s\n", regName[li.storeInReg], regName[ri.storeInReg]);
                    strcpy(regContent[li.storeInReg], "###");
                    retval = lv | rv;
                }
                else if (strcmp(root->lexeme, "^") == 0){
                    printf("XOR %s %s\n", regName[li.storeInReg], regName[ri.storeInReg]);
                    strcpy(regContent[li.storeInReg], "###");
                    retval = lv ^ rv;
                }
                finalEvl.storeInReg = li.type != 'a'? li.storeInReg : ri.storeInReg;
                regInUse[ri.storeInReg] = 0;
                break;
            default:
                retval = 0;
                break;
        }
    }
    finalEvl.value = retval;
    if (mode == 'm'){
        if (li.storeInReg != -1){
            regInUse[li.storeInReg] = 0;
            regInUse[ri.storeInReg] = 0;
        }
    }
    return finalEvl;
}

int getval(void)
{
    int i, retval, found;

    if (match(INT)) {
        retval = atoi(getLexeme());
    } else if (match(ID)) {
        i = 0;
        found = 0;
        retval = 0;
        while (i<sbcount && !found) {
            if (strcmp(getLexeme(), table[i].name)==0) {
                retval = table[i].val;
                found = 1;
                break;
            } else {
                i++;
            }
        }
        if (!found) {
            // error(NOTFOUND);
            if (sbcount < TBLSIZE) {
                strcpy(table[sbcount].name, getLexeme());
                table[sbcount].val = 0;
                sbcount++;
            } else {
                error(RUNOUT);
            }
        }
    }
    return retval;
}

int setval(char *str, int val)
{
    int i, retval = 0;
    i = 0;
    while (i<sbcount) {
        if (strcmp(str, table[i].name)==0) {
            table[i].val = val;
            retval = val;
            break;
        } else {
            i++;
        }
    }
    return retval;
}

//  expr        := term expr_tail
//  expr_tail   := BITOR term expr_tail | NIL
BTNode* expr(void)
{
    BTNode *retp, *left;
    retp = left = term();
    while (match(BITOR)) {  // tail recursion => while
        // printf("expr_tail ");
        retp = makeNode(BITOR, getLexeme());
        advance();
        retp->right = term();
        retp->left = left;
        left = retp;
    }
    return retp;
}

//  term        := term2 term_tail
//  term_tail   := BITXOR term2 term_tail | NIL
BTNode* term(void)
{
    BTNode *retp, *left;
    retp = left = term2();
    while (match(BITXOR)) {  // tail recursion => while
        // printf("expr_tail ");
        retp = makeNode(BITXOR, getLexeme());
        advance();
        retp->right = term2();
        retp->left = left;
        left = retp;
    }
    return retp;
}

//  term2       := term3 term2_tail
//  term2_tail  := BITAND term3 term2_tail | NIL
BTNode* term2(void)
{
    BTNode *retp, *left;
    retp = left = term3();
    while (match(BITAND)) {  // tail recursion => while
        // printf("expr_tail ");
        retp = makeNode(BITAND, getLexeme());
        advance();
        retp->right = term3();
        retp->left = left;
        left = retp;
    }
    return retp;
}

//  term3       := term4 term3_tail
//  term3_tail  := ADDSUB term4 term3_tail | NIL
BTNode* term3(void)
{
    BTNode *retp, *left;
    retp = left = term4();
    while (match(ADDSUB)) {  // tail recursion => while
        // printf("expr_tail ");
        retp = makeNode(ADDSUB, getLexeme());
        advance();
        retp->right = term4();
        retp->left = left;
        left = retp;
    }
    return retp;
}

//term4       := factor term4_tail
//term4_tail  := MULDIV factor term4_tail | NIL
BTNode* term4(void)
{
    BTNode *retp, *left;
    retp = left = factor();
    while (match(MULDIV)) { // tail recursion => while
        // printf("term_tail ");
        retp = makeNode(MULDIV, getLexeme());
        advance();
        retp->right = factor();
        if (strcmp(retp->lexeme, "/") == 0 && retp->right->val == 0){
            if (retp->right->token == INT){
                error(NAN);
            }
        }
        retp->left = left;
        left = retp;
    }
    return retp;
}

BTNode* factor(void)
{
    BTNode* retp = NULL;
    char tmpstr[MAXLEN];

    if (match(INT)) {
        idNumCount++;
        retp =  makeNode(INT, getLexeme());
        retp->val = getval();
        advance();
    } else if (match(ID)) {
        idNumCount++;
        BTNode* left = makeNode(ID, getLexeme());
        left->val = getval();
        strcpy(tmpstr, getLexeme());
        advance();
        if (match(ASSIGN)) {
            idNumCount--;
            if (idNumCount != 0){
                error(SYNERR);
            }
            assignAppeared = 1;
            retp = makeNode(ASSIGN, getLexeme());
            advance();
            retp->right = expr();
            retp->left = left;
        } else {
            retp = left;
        }
    } else if (match(ADDSUB)) {
        idNumCount++;
        strcpy(tmpstr, getLexeme());
        advance();
        if (match(ID) || match(INT)) {
            if (match(ID)){
                retp = makeNode(ID, getLexeme());
            }
            else {
                retp = makeNode(INT, getLexeme());
            }
            if (strcmp(tmpstr, "+") == 0){
                retp->val = getval();
            }
            else {
                retp->sign = 1;
                retp->val = -getval();
            }
            advance();
        } else {
            error(NOTNUMID);
        }
    } else if (match(LPAREN)) {
        reduceCntStack[++top] = idNumCount;
        idNumCount = 0;
        advance();
        retp = expr();
        if (match(RPAREN)) {
            idNumCount += reduceCntStack[top--];
            advance();
        } else {
            error(MISPAREN);
        }
    } else {
        error(NOTNUMID);
    }
    return retp;
}

void error(ErrorType errorNum)
{
    switch (errorNum) {
        case MISPAREN:
            printf("EXIT 1\n");
            fprintf(stderr, "Mismatched parenthesis\n");
            break;
        case NOTNUMID:
            printf("EXIT 1\n");
            fprintf(stderr, "Number or identifier expected\n");
            break;
        case NOTFOUND:
            printf("EXIT 1\n");
            fprintf(stderr, "variable not defined\n");
            break;
        case RUNOUT:
            printf("EXIT 1\n");
            fprintf(stderr, "Out of memory\n");
            break;
        case NAN:
            printf("EXIT 1\n");
            fprintf(stderr, "Not a number\n");
            break;
        case SYNERR:
            printf("EXIT 1\n");
            fprintf(stderr, "Syntax Error\n");
            break;
        case FINISH:
            printf("MOV r0 [0]\n");
            printf("MOV r1 [4]\n");
            printf("MOV r2 [8]\n");
            printf("EXIT 0\n");
            break;
    }
    exit(0);
}

void statement(void)
{
    BTNode* retp;
    // printf("statement\n");
    if (match(END)) {
        // printf(">> ");
        advance();
    }
    else if (match(FIN)){
        error(FINISH);
    } else {
        retp = expr();
        if (!assignAppeared){
            error(SYNERR);
        }
        if (match(END)) {
            printf("%d\n", evaluateTree(retp, 'm').value);
            // printPrefix(retp);
            // printf("\n");
            // evaluateTree(retp, 'm');
            freeTree(retp);

            // printf(">> ");
            advance();
        }
        else {
            // printf("match %d\n", lkahead());
            error(SYNERR);
        }
    }
}

int main()
{
    strcpy(memory[0], "x");
    strcpy(memory[1], "y");
    strcpy(memory[2], "z");
    volatileMem[0] = 1;
    volatileMem[1] = 1;
    volatileMem[2] = 1;

    //freopen("..\\test.txt", "w", stdout);
    //freopen("..\\testcase\\15.in", "r", stdin);

    // printf(">> ");
    while (1) {
        idNumCount = 0;
        top = -1;
        usedMemInCurStat = 0;
        assignAppeared = 0;
        for (int i = 0; i < 8; i++){
            strcpy(regContent[i], "###");
        }

        statement();
    }
    printf("EXIT 0\n");
    return 0;
}

