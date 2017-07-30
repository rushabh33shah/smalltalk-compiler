package smalltalk.compiler;

import org.antlr.symtab.*;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.TerminalNode;
import smalltalk.compiler.symbols.*;
import java.util.List;


/** Fill STBlock, STMethod objects in Symbol table with bytecode,
 * {@link STCompiledBlock}.
 */
public class CodeGenerator extends SmalltalkBaseVisitor<Code> {
	public static final boolean dumpCode = false;

	public STClass currentClassScope;
	public Scope currentScope;

	/** With which compiler are we generating code? */
	public final Compiler compiler;

	public CodeGenerator(Compiler compiler) {
		this.compiler = compiler;
	}

	/** This and defaultResult() critical to getting code to bubble up the
	 *  visitor call stack when we don't implement every method.
	 */
	@Override
	protected Code aggregateResult(Code aggregate, Code nextResult) {
		if ( aggregate!=Code.None ) {
			if ( nextResult!=Code.None ) {
				return aggregate.join(nextResult);
			}
			return aggregate;
		}
		else {
			return nextResult;
		}
	}

	@Override
	protected Code defaultResult() {
		return Code.None;
	}

	@Override
	public Code visitFile(SmalltalkParser.FileContext ctx) {
		currentScope = compiler.symtab.GLOBALS;
		visitChildren(ctx);
		return Code.None;
	}

	@Override
	public Code visitClassDef(SmalltalkParser.ClassDefContext ctx) {
		currentClassScope = ctx.scope;
		pushScope(ctx.scope);
		visitChildren(ctx);
		popScope();
		currentClassScope = null;
		return Code.None;
	}

	public STCompiledBlock getCompiledPrimitive(STPrimitiveMethod primitive) {
		STCompiledBlock compiledMethod = new STCompiledBlock(currentClassScope, primitive);
		return compiledMethod;
	}

	/**
	 All expressions have values. Must pop each expression value off, except
	 last one, which is the block return value. Visit method for blocks will
	 issue block_return instruction. Visit method for method will issue
	 pop self return.  If last expression is ^expr, the block_return or
	 pop self return is dead code but it is always there as a failsafe.

	 localVars? expr ('.' expr)* '.'?
	 */
	@Override
	public Code visitFullBody(SmalltalkParser.FullBodyContext ctx) {
		// fill in
        Code code = new Code();

        if(ctx.localVars() != null) {
            code = aggregateResult(code,visitLocalVars(ctx.localVars()));
        }
		List<SmalltalkParser.StatContext> stats = ctx.stat();
		for (int i = 0; i < stats.size(); i++) {
			code = aggregateResult(code,visit(ctx.stat(i)));
		     if (i < stats.size() - 1)
			    code = aggregateResult(code,Compiler.pop());
        }
		return code;
	}

	@Override
	public Code visitReturn(SmalltalkParser.ReturnContext ctx) {

	    Code e = visit(ctx.messageExpression());
        Code code = aggregateResult(e,Compiler.method_return());

        return code;
	}

	public void pushScope(Scope scope) {
		currentScope = scope;
	}

	public void popScope() {
		currentScope = currentScope.getEnclosingScope();
	}

	public int getLiteralIndex(String s) {

        String tempStr = s.replace("\'","");
        int i = currentClassScope.stringTable.add(tempStr);
        return i;
	}

	public Code dbgAtEndMain(Token t) {
		int charPos = t.getCharPositionInLine() + t.getText().length();
		return dbg(t.getLine(), charPos);
	}

	public Code dbgAtEndBlock(Token t) {
		int charPos = t.getCharPositionInLine() + t.getText().length();
		charPos -= 1; // point at ']'
		return dbg(t.getLine(), charPos);
	}

	public Code dbg(Token t) {
		//return dbg(t.getLine(), t.getCharPositionInLine());
        return Code.None;
	}

	public Code dbg(int line, int charPos) {
		return Code.None;//Compiler.dbg(getLiteralIndex(compiler.getFileName()), line, charPos);
	}

	public Code store(String id) {
        Code code = new Code();
        Symbol symbol = currentScope.resolve(id);
        if ( symbol instanceof STField ) {
            code = aggregateResult(code,Compiler.store_field(symbol.getInsertionOrderNumber()));
        }
        else if ( symbol instanceof STVariable) {
            int i = symbol.getInsertionOrderNumber();
            int d = ((STBlock) currentScope).getRelativeScopeCount(symbol.getScope().getName());
            code = aggregateResult(code,Compiler.store_local(d, i));
        }
        else {
            return Code.None;
        }
        return code;
    }

	public Code push(String id) {
        Code code = new Code();
        Symbol symbol = currentScope.resolve(id);
        if (symbol == null || symbol.getScope() == compiler.symtab.GLOBALS) {

            int index = getLiteralIndex(id);
            code = aggregateResult(code,Compiler.push_global(index));
        }else {
            if (symbol instanceof STField) {
                int index = ((STClass)symbol.getScope()).getFieldIndex(id);
                try {
                    int temp = ((STClass) symbol.getScope()).getSuperClassScope().getNumberOfFields();
                    code = aggregateResult(code,Compiler.push_field(index + temp));
                }
                catch (NullPointerException e) {
                    code = aggregateResult(code,Compiler.push_field(index));
                }
            } else {
                int i = symbol.getInsertionOrderNumber();
                int d = ((STBlock) currentScope).getRelativeScopeCount(symbol.getScope().getName());
                code = aggregateResult(code,Compiler.push_local(d, i));
            }
        }
        return code;
	}

	public Code sendKeywordMsg(ParserRuleContext receiver,
							   Code receiverCode,
							   List<SmalltalkParser.BinaryExpressionContext> args,
							   List<TerminalNode> keywords)
	{
		return null;
	}

	public String getProgramSourceForSubtree(ParserRuleContext ctx) {
		return null;
	}

	@Override
	public Code visitKeywordSend(SmalltalkParser.KeywordSendContext ctx) {
        Code code = visit(ctx.recv);
        Code code1 = new Code();
        if (ctx.args.size() != 0){
            for(int i = 0; i < ctx.args.size(); i++){
               aggregateResult(code1,visit(ctx.args.get(i)));
            }
        }

        if (ctx.KEYWORD().size() != 0){
            String str = "";
            for (int i = 0; i < ctx.KEYWORD().size(); i++){
                str += ctx.KEYWORD(i).getText();
            }
            int argIndex = getLiteralIndex(str);
            int argSize = ctx.args.size();
            aggregateResult(code1,Compiler.send(argSize, argIndex));
        }
        aggregateResult(code, code1);
        return code;
	}

	@Override
	public Code visitBinaryExpression(SmalltalkParser.BinaryExpressionContext ctx) {
        Code code =  visit(ctx.unaryExpression(0));
            String str;
            for (int i = 1 ; i <= ctx.bop().size();i++){

                code = aggregateResult(code, visit(ctx.unaryExpression(i)));
                str = ctx.bop().get(i-1).getText();
                int index = getLiteralIndex(str);
                code = aggregateResult(code,Compiler.send(1,index));
            }
        return code;
	}

    @Override
    public Code visitLiteral(SmalltalkParser.LiteralContext ctx) {

        Code code = new Code();
        if (ctx.CHAR() != null){
            char c = ctx.CHAR().getText().charAt(1);
            code = aggregateResult(code,Compiler.push_char(c));
        }
        else if (ctx.STRING() != null ){
            String s = ctx.STRING().getText();
            int index = getLiteralIndex(s);
            code = aggregateResult(code,Compiler.push_literal(index));
        }
        else if (ctx.NUMBER() != null){
            String num = ctx.NUMBER().getText();
            if( num.contains(".") ){
                float f = Float.parseFloat(num);
              code = aggregateResult(code,Compiler.push_float(f));
            }else {
                int i = Integer.parseInt(num);
                code = aggregateResult(code,Compiler.push_int(i));
            }
        } else {
            String s = ctx.getText();
            if(s.equals("nil"))
                code = aggregateResult(code,Compiler.push_nil());
            if(s.equals("self"))
                code = aggregateResult(code,Compiler.push_self());
            if(s.equals("true"))
                code = aggregateResult(code,Compiler.push_true());
            if(s.equals("false"))
                code = aggregateResult(code,Compiler.push_false());
        }
        return code;
    }

    @Override
    public Code visitAssign(SmalltalkParser.AssignContext ctx) {

        Code e = visit(ctx.messageExpression());
        Code store = store(ctx.lvalue().ID().getText());
        Code code = aggregateResult(e, store);

        return code;
    }

    @Override
    public Code visitMain(SmalltalkParser.MainContext ctx) {
        currentClassScope = ctx.classScope;
        if(ctx.body() == null || ctx.body().getChildCount() == 0) {
            return Code.None;
        }
        pushScope(ctx.classScope);
        pushScope(ctx.scope);
        Code  code = visitChildren(ctx);

        STCompiledBlock stCompiledBlock = new STCompiledBlock(currentClassScope, (STBlock) currentScope);
        ctx.scope.compiledBlock = stCompiledBlock;
        code = aggregateResult(code,Compiler.pop());
        code = aggregateResult(code,Compiler.push_self());
        code = aggregateResult(code,Compiler.method_return());
        ctx.scope.compiledBlock.bytecode = code.bytes();

        if(ctx.scope.isMethod()) {
            List<Scope> STB = ctx.scope.getAllNestedScopedSymbols();
            ctx.scope.compiledBlock.blocks = new STCompiledBlock[STB.size()];
            for (int i = 0; i < STB.size(); i++) {
                STBlock stb = ((STBlock) STB.get(i));
                ctx.scope.compiledBlock.blocks[stb.index] = stb.compiledBlock;
            }
        }
        popScope();
        popScope();

        return code;
    }

    @Override
    public Code visitId(SmalltalkParser.IdContext ctx) {
        Code code = new Code();
        code = aggregateResult(code,push(ctx.getText()));
        return code;
    }

    @Override
    public Code visitBlock(SmalltalkParser.BlockContext ctx) {

        pushScope(ctx.scope);
        Code IC = new Code();
        short Index = (short)ctx.scope.index;
        IC = aggregateResult (IC,Compiler.block(Index));
        Code code = visitChildren(ctx);

        if (ctx.body() instanceof SmalltalkParser.EmptyBodyContext){
            code = aggregateResult(code,Compiler.push_nil());
        }
        code = aggregateResult(code,Compiler.block_return());
        STCompiledBlock stCompiledBlock = new STCompiledBlock(currentClassScope,ctx.scope);
        stCompiledBlock.bytecode = code.bytes();
        ctx.scope.compiledBlock = stCompiledBlock;
        popScope();

        return IC;
    }

    @Override
    public Code visitPrimitiveMethodBlock(SmalltalkParser.PrimitiveMethodBlockContext ctx) {

        SmalltalkParser.MethodContext methodContext = (SmalltalkParser.MethodContext)ctx.getParent();
        pushScope(methodContext.scope);
        Code code = visitChildren(ctx);
        STCompiledBlock stCompiledBlock = new STCompiledBlock(currentClassScope,methodContext.scope);
        stCompiledBlock.bytecode = code.bytes();
        methodContext.scope.compiledBlock = stCompiledBlock;
        popScope();

        return Code.None;
    }

    @Override
    public Code visitSmalltalkMethodBlock(SmalltalkParser.SmalltalkMethodBlockContext ctx) {
        SmalltalkParser.MethodContext methodContext = (SmalltalkParser.MethodContext)ctx.getParent();
        pushScope(methodContext.scope);
        Code code = visitChildren(ctx);

        if ( ctx.body() instanceof SmalltalkParser.FullBodyContext ) {
            code = aggregateResult(code,Compiler.pop());
        }
        code = aggregateResult(code,Compiler.push_self());
        code = aggregateResult(code,Compiler.method_return());
        STCompiledBlock stCompiledBlock = new STCompiledBlock(currentClassScope,methodContext.scope);
        stCompiledBlock.bytecode = code.bytes();
        methodContext.scope.compiledBlock = stCompiledBlock;
        addToBlock(methodContext);
        popScope();

        return Code.None;
    }

    @Override
    public Code visitSuperKeywordSend(SmalltalkParser.SuperKeywordSendContext ctx) {
        Code code = new Code();
        if (ctx.args.size() != 0) {
            for (int i = 0; i < ctx.args.size(); i++) {
                code = aggregateResult(code,visit(ctx.args.get(i)));
            }
        }
        if (ctx.KEYWORD().size() != 0) {
            String str = "";
            for (int i = 0; i < ctx.KEYWORD().size(); i++) {
                str += ctx.KEYWORD(i).getText();
            }
            int Index = getLiteralIndex(str);
            int Size = ctx.args.size();
            code.join(Compiler.send(Size, Index));
        }
        return code;
    }

    @Override
    public Code visitUnaryMsgSend(SmalltalkParser.UnaryMsgSendContext ctx) {
        Code code = visit(ctx.unaryExpression());
        String str = ctx.ID().getText();
        int index = getLiteralIndex(str);
        code.join(Compiler.send(0,index));

        return code;
    }

    @Override
    public Code visitUnarySuperMsgSend(SmalltalkParser.UnarySuperMsgSendContext ctx) {
        Code code = new Code();
        String str = ctx.ID().getText();
        int index = getLiteralIndex(str);
        code.join(Compiler.push_self()).join(Compiler.send_super(0, index));

        return code;
    }

    public void addToBlock(SmalltalkParser.MethodContext ctx){
        List<Scope> STBlocks = ctx.scope.getAllNestedScopedSymbols();
        ctx.scope.compiledBlock.blocks = new STCompiledBlock[STBlocks.size()];
        for (int i = 0; i < STBlocks.size(); i++) {
            STBlock stb = ((STBlock) STBlocks.get(i));
            ctx.scope.compiledBlock.blocks[stb.index] = stb.compiledBlock;
        }
    }
}
