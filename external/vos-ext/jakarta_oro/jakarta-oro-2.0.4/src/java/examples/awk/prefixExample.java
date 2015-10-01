/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2000 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution,
 *    if any, must include the following acknowledgment:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgment may appear in the software itself,
 *    if and wherever such third-party acknowledgments normally appear.
 *
 * 4. The names "Apache" and "Apache Software Foundation", "Jakarta-Oro" 
 *    must not be used to endorse or promote products derived from this
 *    software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache" 
 *    or "Jakarta-Oro", nor may "Apache" or "Jakarta-Oro" appear in their 
 *    name, without prior written permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 * Portions of this software are based upon software originally written 
 * by Daniel F. Savarese. We appreciate his contributions.
 */

/*
 * $Id: prefixExample.java,v 1.4 2001/05/20 23:55:16 dfs Exp $
 */
import org.apache.oro.text.regex.*;
import org.apache.oro.text.awk.*;

/**
 * This is a test program demonstrating an application of the matchesPrefix()
 * methods.  This example program shows how you might tokenize a stream of
 * input using whitespace as a token separator.  Don't forget to use quotes
 * around the input on the command line, e.g.
 *    java prefixExample "Test to see if 1.0 is real and 2 is an integer"
 *
 * If you don't need the power of a full blown lexer generator, you can
 * easily use regular expressions to create your own tokenization and
 * simple parsing classes using similar approaches.  This example is
 * rather sloppy.  If you look at the equivalent example in the OROMatcher
 * distribution, you'll see how to Perl's zero-width look ahead assertion
 * makes correctness easier to achieve.
 *
 * @author <a href="mailto:oro-dev@jakarta.apache.org">Daniel F. Savarese</a>
 * @version @version@
 */
public final class prefixExample {
  public static final int REAL        = 0;
  public static final int INTEGER     = 1;
  public static final int STRING      = 2;

  public static final String[] types = { "Real", "Integer", "String" };
  public static final String whitespace = "[ \t\n\r]+";
  public static final String[] tokens   = {
    "-?[0-9]*\\.[0-9]+([eE]-?[0-9]+)?", "-?[0-9]+", "[^ \t\n\r]+"
  };

  public static final void main(String args[]) {
    int token;
    PatternMatcherInput input;
    PatternMatcher matcher;
    PatternCompiler compiler;
    Pattern[] patterns;
    Pattern tokenSeparator = null;
    MatchResult result;

    if(args.length < 1) {
      System.err.println("Usage: prefixExample <sample input>");
      System.exit(1);
    }

    input    = new PatternMatcherInput(args[0]);
    compiler = new AwkCompiler();
    patterns = new Pattern[tokens.length];

    try {
      tokenSeparator = compiler.compile(whitespace);
      for(token=0; token < tokens.length; token++)
	patterns[token] = compiler.compile(tokens[token]);
    } catch(MalformedPatternException e) {
      System.err.println("Bad pattern.");
      e.printStackTrace();
      System.exit(1);
    }

    matcher  = new AwkMatcher();

  _whileLoop:
    while(!input.endOfInput()) {
      for(token = 0; token < tokens.length; token++)
	if(matcher.matchesPrefix(input, patterns[token])) {
	  int offset;
	  result = matcher.getMatch();
	  offset = input.getCurrentOffset();
	  input.setCurrentOffset(result.endOffset(0));

	  if(matcher.matchesPrefix(input, tokenSeparator)) {
	    input.setCurrentOffset(matcher.getMatch().endOffset(0));
	    System.out.println(types[token] + ": " + result);
	    continue _whileLoop;
	  } else if(input.endOfInput()) {
	    System.out.println(types[token] + ": " + result);
	    break _whileLoop;
	  }

	  input.setCurrentOffset(offset);
	}

      if(matcher.matchesPrefix(input, tokenSeparator))
	input.setCurrentOffset(matcher.getMatch().endOffset(0));
      else {
	System.err.println("Unrecognized token starting at offset: " +
			   input.getCurrentOffset());
	break;
      }
    }

  }
}
