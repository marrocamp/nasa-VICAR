#include <math.h>
#include "pdsdef.h"
#include "formtype.h"
#include "formtype2.h"

/*************************************************************************************
 *																					 *
 *	Change History:																	 *
 *																					 *
 *		08-09-02  MDC																 *
 *			Added a check in the "ft_msb_ieee_real" function						 *
 *			that makes sure that the real value obtained is not bigger than			 *
 *			the amount of bytes allocated to the converted_text variable it			 *
 *			gets stored into. Originally, the program crashed when a value stored	 *
 *			into the 'converted_text' variable exceeded its allocated space.		 *
 *			Thus, this added check feature prevents the program from crashing.		 *
 *																					 *
 *	    08-29-02 MDC																 *
 *			Added functionality to handle an 8 byte unsigned int in the				 *
 *			ft_msb_integer routine. Added an ftx_interpret_64_bits routine in		 *
 *			the program as well to be able to handle an 8 byte integer.				 *
 *																					 *
 *		10-13-02 MDC																 *	  
 *		 1) Added functionality to handle a 6-byte MSB integer in the ft_msb_integer * 
 *			routine.																 *
 *		 2) Added a check in the stringvalfmt routine to return a "0" string if      *
 *			the sum array passed into this routine is all 0's. This will prevent 	 *
 *			tbtool from crashing if this case occurs.								 *
 *		 3) Added functionality to handle a 6-byte LSB integer in the ft_lsb_integer *
 *			routine.																 *
 *																					 *
 *      08-21-03 MDC                                                                 *
 *         Modified ft_lsb_integer and ft_msb_integer routines. See notes.           *
 *************************************************************************************/


#ifndef SUN_UNIX

static long ftx_interpret_bits (unsigned char, long *, long, long, LOGICAL );

/*--------------------------------------------------------------------*/
/** 08.30.2002 MDC													 **/ 
/** Added this routine to interpret an integer that is 8 bytes.		 **/
/**																	 **/
/** 10-10-02 MDC													 **/
/** This routine is now used to handle 6-byte integers.				 **/
/*--------------------------------------------------------------------*/
void ftx_interpret_64_bits (short [], unsigned char, long, long, long, LOGICAL );

/*--------------------------------------------------------------------*/
/** 09.06.2002 MDC													 **/
/** Added the calc_value routine to be able to store, interpret, and **/
/** handle an 8-byte integer.										 **/
/*--------------------------------------------------------------------*/
void calc_value(short [], short, short, short, LOGICAL );

/*--------------------------------------------------------------------*/
/** 09.06.2002 MDC													 **/ 
/** Added the stringvalfmt routine to be able to put all the digits  **/
/** stored in the array into a string.								 **/
/*--------------------------------------------------------------------*/
unsigned char *stringvalfmt(short [], LOGICAL);

static double ftx_interpret_fraction (unsigned char byte, double *inverse_pow2, long start_bit, long stop_bit);

#else

static long ftx_interpret_bits (unsigned char, long *, long, long, LOGICAL);
void ftx_interpret_64_bits(short [], unsigned char, long, long, long, LOGICAL);
void calc_value(short [], short, short, short, LOGICAL);
unsigned char *stringvalfmt(short [], LOGICAL);
static double ftx_interpret_fraction (unsigned char byte, double *inverse_pow2, long start_bit, long stop_bit);

#endif

/*****/

unsigned char *ft_char (text, size)

unsigned char *text;
long size;

{
    unsigned char *converted_text = {NULL};
    unsigned char *t = {NULL};
    unsigned char *c = {NULL};
    long i;

    U_Malloc_String(converted_text, (int) size + 1);
    for (i=0,t=text,c=converted_text; i < size; ++i, ++t, ++c)
    {
        if (! isprint(*t))
            *c = '.';
        else
            *c = *t;
    }
    *c = EOS;

    return (converted_text);

}  /*  "ft_char"  */



/*****/

unsigned char *ft_hex (text, size)

unsigned char *text;
long size;

{
    unsigned char *converted_text = {NULL};
    unsigned char *t = {NULL};
    unsigned char *c = {NULL};
    long i;

    U_Malloc_String(converted_text, (int) (2*(size + (int) (size/4) + 1)));
    for (i=0,t=text,c=converted_text; i < size; ++i, ++t, c += 2)
    {
        if (((i % 4) == 0) && (i > 0))
        {
            *c = ' ';
            ++c;
        }
        if (*t < 16)
            sprintf (c, "0%x", *t);
        else                        
            sprintf (c, "%x", *t);
    }                            
    *c = EOS;

    return (converted_text);

}  /*  "ft_hex" */



/*****/

unsigned char *ft_lsb_binary (text, size)

unsigned char *text;
long size;

{
    BYTE_CONVERSION convert;
    unsigned char *converted_text = {NULL};
    unsigned char *t = {NULL};
    unsigned char *c = {NULL};
    long i;

    U_Malloc_String(converted_text, (int) (9*size) + 1);
    for (i=0,t=(text+size-1),c=converted_text; i < size; ++i, --t, ++c)
    {
        if (i > 0) 
        { 
            *c = ' '; ++c; 
        }
        convert.byte = *t;
        sprintf (c, "%d", (convert.bit.b0 != 0)); ++c;
        sprintf (c, "%d", (convert.bit.b1 != 0)); ++c;
        sprintf (c, "%d", (convert.bit.b2 != 0)); ++c;
        sprintf (c, "%d", (convert.bit.b3 != 0)); ++c;
        sprintf (c, "%d", (convert.bit.b4 != 0)); ++c;
        sprintf (c, "%d", (convert.bit.b5 != 0)); ++c;
        sprintf (c, "%d", (convert.bit.b6 != 0)); ++c;
        sprintf (c, "%d", (convert.bit.b7 != 0));
    }
    *c = EOS;

    return (converted_text);

}  /*  "ft_lsb_binary"  */



/*****/

unsigned char *ft_lsb_ieee_complex (text, size)

unsigned char *text;
long size;

{
    unsigned char *converted_text = {NULL};
    unsigned char *r_value = {NULL};
    unsigned char *c_value = {NULL};
    long c_size;

    c_size = (long) (size/2);

    r_value = ft_lsb_ieee_real (text, c_size);
    c_value = ft_lsb_ieee_real ((text + c_size), c_size);

    if ((r_value != NULL) && (c_value != NULL))
    { 
        U_Malloc_String(converted_text, 2*PDS_NUMBER_SIZE)
        sprintf(converted_text, "%s %s", r_value, c_value);
    }

    Lemme_Go(r_value)
    Lemme_Go(c_value)

    return (converted_text);

}  /*  "ft_lsb_ieee_complex"  */


        
/*****/

unsigned char *ft_lsb_ieee_real (text, size)

unsigned char *text;
long size;

{
    unsigned char *converted_text = {NULL};
    long pow2 = {1};
    unsigned long biased_exp = {0};
    long exponent = {0};
    double inv_pow2 = {0.5};
    double mantissa = {1.0};
    double f_num = {0.0};
    LOGICAL exponent_overflow = {FALSE};

    /*--------------------------------------------------------------------
     *
     *   For LSB IEEE 4-byte real numbers:
     *
     *      e0-bit  = lowest order bit of the exponent
     *      e1      = seven high order bits of the exponent
     *      m0 - m2 = order of bytes in mantissa (m0 = leftmost)
     *                (note that m0 has only seven bits)
     *      m-sign  = mantissa sign bit
     *      b0 - b3 = order of bytes in text string
     *
     *                      e0-bit
     *                      |       m-sign
     *                      |       |
     *         m2      m1   |   m0  |   e1
     *      v------vv------v|v-----v|v-----v
     *      76543210765432107654321076543210
     *      |------||------||------||------|
     *         b0      b1      b2      b3
     *
     *      exponent bias = 127
     *
     *   To interpret this as a more typical floating point representation,
     *   (e.g., sign-exponent-mantissa form) you must reverse the order of
     *   the bytes: b3 b2 b1 b0
     *
     *--------------------------------------------------------------------
     *
     *   For LSB IEEE 8-byte (double precision) real numbers:
     *
     *      e0      = four low order bits of the exponent
     *      e1      = seven high order bits of the exponent
     *      m0 - m6 = order of bytes in mantissa (m0 = leftmost)
     *                (note that m0 has only four bits)
     *      m-sign  = mantissa sign bit
     *      b0 - b7 = order of bytes in text string
     *    
     *      exponent bias = 1023
     *
     *                                                            m-sign
     *                                                            |
     *       m6      m5      m4      m3      m2      m1    e0  m0 |   e1
     *    v------vv------vv------vv------vv------vv------vv--vv--v|v-----v 
     *    7654321076543210765432107654321076543210765432107654321076543210
     *    |------||------||------||------||------||------||------||------|
     *       b0      b1      b2      b3      b4      b5      b6      b7
     *
     *   To interpret this as a more typical floating point representation,
     *   (e.g., sign-exponent-mantissa form) you must reverse the order of
     *   the bytes: b7 b6 b5 b4 b3 b2 b1 b0
     *
     *--------------------------------------------------------------------
     *
     *   For MSB IEEE 10-byte (temporary) real numbers:
     *
     *      e0      = eight low order bits of the exponent
     *      e1      = seven high order bits of the exponent
     *      m0 - m7 = order of bytes in mantissa (m0 = leftmost)
     *                (note that m0 has only seven bits)
     *      m-sign  = mantissa sign bit
     *      int-bit = integer bit ("1." part) of mantissa
     *      b0 - b9 = order of bytes in text string
     *    
     *      exponent bias = 16383
     *
     *       m7      m6      m5      m4      m3
     *    v------vv------vv------vv------vv------v
     *    7654321076543210765432107654321076543210
     *    |------||------||------||------||------|
     *       b0      b1      b2      b3      b4
     *
     *                    int-bit         m-sign
     *                    |               |
     *       m2      m1   |   m0     e0   |   e1
     *    v------vv------v|v-----vv------v|v-----v
     *    7654321076543210765432107654321076543210
     *    |------||------||------||------||------|
     *       b5      b6      b7      b8      b9
     *
     *   To interpret this as a more typical floating point representation,
     *   (e.g., sign-exponent-mantissa form) you must reverse the order of
     *   the bytes: b9 b8 b7 b6 b5 b4 b3 b2 b1 b0
     *
     *--------------------------------------------------------------------
     *
     *   These representations all follow the format:
     *
     *       1.(mantissa) x 2**(exponent - bias)
     *
     *   with the "1." part implicit (except for the 10-byte temp real, 
     *   in which the "1." part is actually stored in the eighth byte (b7)),
     *
     *   In all cases, the exponent is stored as an unsigned, biased
     *   integer.
     *          
     *--------------------------------------------------------------------
     */

    if ((size == PDS_IEEE_REAL_BYTES) ||
          (size == PDS_IEEE_DOUBLE_BYTES) || (size == PDS_IEEE_TEMP_BYTES))
    {
        if (size == PDS_IEEE_REAL_BYTES)
        {
            pow2 = 1;  
            biased_exp += ftx_interpret_bits (text[2], &pow2, 7, 7, FALSE);
            biased_exp += ftx_interpret_bits (text[3], &pow2, 0, 6, FALSE);
            exponent = biased_exp - 127;

            if (exponent > PDS_MAX_EXPONENT)
                exponent_overflow = TRUE;
            else
            {
                inv_pow2 = 0.5;
                mantissa += ftx_interpret_fraction (text[2], &inv_pow2, 6, 0);
                mantissa += ftx_interpret_fraction (text[1], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[0], &inv_pow2, 7, 0);
		f_num = (text[3]&128) ? - ldexp (mantissa, (int) exponent) :
					  ldexp (mantissa, (int) exponent);
            }
        }
        else
        if (size == PDS_IEEE_DOUBLE_BYTES)
        {
            pow2 = 1;
            biased_exp += ftx_interpret_bits (text[6], &pow2, 4, 7, FALSE);
            biased_exp += ftx_interpret_bits (text[7], &pow2, 0, 6, FALSE);
            exponent = biased_exp - 1023;  

            if (exponent > PDS_MAX_EXPONENT)
                exponent_overflow = TRUE;
            else
            {
                inv_pow2 = 0.5;
                mantissa += ftx_interpret_fraction (text[6], &inv_pow2, 3, 0);
                mantissa += ftx_interpret_fraction (text[5], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[4], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[3], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[2], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[1], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[0], &inv_pow2, 7, 0);
		f_num = (text[7]&128) ? - ldexp (mantissa, (int) exponent) :
					  ldexp (mantissa, (int) exponent);
            }
        }
        else
        if (size == PDS_IEEE_TEMP_BYTES)
        {
            pow2 = 1;
            biased_exp += ftx_interpret_bits (text[8], &pow2, 0, 7, FALSE);
            biased_exp += ftx_interpret_bits (text[9], &pow2, 0, 6, FALSE);
            exponent = biased_exp - 16383;

            if (exponent > PDS_MAX_EXPONENT)
                exponent_overflow = TRUE;
            else
            {
                inv_pow2 = 0.5;
                mantissa += ftx_interpret_fraction (text[7], &inv_pow2, 6, 0);
                mantissa += ftx_interpret_fraction (text[6], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[5], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[4], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[3], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[2], &inv_pow2, 7, 0);

                /* IEEE mantissas have 52 bits */
                /* VAXD mantissas have 55 bits */
#ifdef VAX
                mantissa += ftx_interpret_fraction (text[1], &inv_pow2, 7, 0);
#else
                mantissa += ftx_interpret_fraction (text[1], &inv_pow2, 7, 3);
#endif
		f_num = (text[9]&128) ? - ldexp (mantissa, (int) exponent) :
					  ldexp (mantissa, (int) exponent);
            }
        }

        if (! exponent_overflow)
        {
            U_Malloc_String(converted_text, PDS_NUMBER_SIZE);
            sprintf (converted_text, "%20.10lf", f_num);
        }

    }  /*  End:  "if ((size == PDS_IEEE_REAL_BYTES) || ..."  */

    return (converted_text);

}  /*  "ft_lsb_ieee_real"  */


        
/*****/

/*********************************************************************/
/* Change History                                                    */
/*    08-21-2003   MDC    Corrected the logic to properly calculate  */
/*                        an unformatted value.                      */
/*********************************************************************/
unsigned char *ft_lsb_integer (text, size, signed_value)

unsigned char *text;
long size;
LOGICAL signed_value;

{
    unsigned char *converted_text = {NULL};
    long byte;
    char num1 = {0};
    short num2 = {0};
    long num4 = {0};
    unsigned char u_num1 = {0};
    unsigned short u_num2 = {0};
    unsigned long u_num4 = {0};
    long pow2 = {1};
	char *result = NULL;
	short sum[MAX_ARRAY_SIZE];
	long x = 0;

	for(x = 0; x < MAX_ARRAY_SIZE; x++)
		sum[x] = 0;

	

	/*-------------------------------------------------------------------
	 *  10-13-02 MDC - Added functionality to handle 6 byte integers
	 *
	 * For 6-byte integers:
	 *
	 *      i0 - i5 = order of bytes in integer (i0 = lowest order)
     *      i-sign  = integer sign bit
     *      b0 - b5 = order of bytes in text string
     *
     *												i-sign
     *												|
     *		   i0     i1      i2      i3	   i4   |  i5      
     *      v------vv------vv------vv------vv------vv------v
     *      765432107654321076543210765432107654321076543210
     *      |------||------||------||------||------||------|
     *         b0      b1      b2      b3      b4      b5      
     *
	 *--------------------------------------------------------------------
     * 09.06.2002 - MDC  Added the ability of this routine to detect and
	 *		handle an 8-byte LSB integer.
	 *
     *  For 8-byte integers:
     *
     *      i0 - i7 = order of bytes in integer (i0 = lowest order)
     *      i-sign  = integer sign bit
     *      b0 - b7 = order of bytes in text string
     *
     *																i-sign
     *																|
     *         i0      i1      i2      i3	   i4	   i5	   i6	|  i7
     *      v------vv------vv------v|v-----vv------vv------vv------vv------v
     *      7654321076543210765432107654321076543210765432107654321076543210
     *      |------||------||------||------||------||------||------||------|
     *         b0      b1      b2      b3	   b4	   b5	   b6	   b7
     *
     *--------------------------------------------------------------------
     *
     *  For 4-byte integers:
     *
     *      i0 - i3 = order of bytes in integer (i0 = lowest order)
     *      i-sign  = integer sign bit
     *      b0 - b3 = order of bytes in text string
     *
     *                              i-sign
     *                              |
     *         i0      i1      i2   |  i3
     *      v------vv------vv------v|v-----v
     *      76543210765432107654321076543210
     *      |------||------||------||------|
     *         b0      b1      b2      b3
     *
     *--------------------------------------------------------------------
     *
     *  For 2-byte integers:
     *
     *              i-sign
     *              |
     *         i0   |  i1   
     *      v------vv------v
     *      7654321076543210
     *      |------||------|
     *         b0      b1   
     *
     *--------------------------------------------------------------------
     *
     *  For 1-byte integers:
     *
     *      i-sign
     *      |
     *      |  i0   
     *      |------v
     *      76543210
     *      |------|
     *         b0   
     *
     *--------------------------------------------------------------------
     *
     *  All negative signed values are assumed to be twos-complement.
     *
     *--------------------------------------------------------------------
     */
	
	/* 09.06.2002 MDC - Added functionality to handle an 8-byte LSB integer. */
    if ((size == PDS_TINY_INT_BYTES) || (size == PDS_SHORT_INT_BYTES) 
			|| (size == PDS_INT_BYTES) || (size == PDS_LONG_INT_BYTES)
			|| (size == PDS_SIX_INT_BYTES) )
    {        
        U_Malloc_String(converted_text, PDS_NUMBER_SIZE);
		
		/*--------------------------------------------------------------------*/
		/** 09-13-02 MDC													 **/	
		/** Changed the last parameter going into the called function,		 **/ 
		/**	ftx_interpret_bits, from "FALSE" to "TRUE". This parameter		 **/ 
		/** indicates whether the integer is signed or unsigned so it can	 **/
		/** perform the proper calculations to obtain the decimal			 **/
		/** representation of the 8-byte integer.							 **/
		/*--------------------------------------------------------------------*/

		/*--------------------------------------------------------------------*/
		/* 08-21-2003 MDC                                                     */
		/* The most significant byte is the only one where the most           */
		/* significant bit is the signed bit. So, only pass in "TRUE" when    */
		/* interpreting the most significant byte to prevent miscalculation   */
		/* of the unformatted value.                                          */
		/*--------------------------------------------------------------------*/
        if (signed_value)
        {  
			if (size == PDS_TINY_INT_BYTES)
            {
                num1 = ftx_interpret_bits (text[0], &pow2, 0, 7, TRUE);
                sprintf (converted_text, "%d", (short) num1);
            }
            else
            if (size == PDS_SHORT_INT_BYTES)
            {                        
				for (byte=0, pow2=1; byte < PDS_SHORT_INT_BYTES; ++byte) {
					if(byte == (PDS_SHORT_INT_BYTES - 1)) {
                        num2 += ftx_interpret_bits (text[byte], &pow2, 0, 7, TRUE);
					}
					else {
						num2 += ftx_interpret_bits (text[byte], &pow2, 0, 7, FALSE);
					}
				}
                sprintf (converted_text, "%d", num2);
            }
            else
            if (size == PDS_INT_BYTES) 
            {
				for (byte=0, pow2=1; byte < PDS_INT_BYTES; ++byte) {
					if(byte == (PDS_INT_BYTES-1)) {
                        num4 += ftx_interpret_bits (text[byte], &pow2, 0, 7, TRUE);
					}
					else {
						num4 += ftx_interpret_bits (text[byte], &pow2, 0, 7, FALSE);
					}
				}
                sprintf (converted_text, "%ld", num4);
            }
			else
			/*---------------------------------------------------------------*/
			/** 09.06.2002 MDC												**/
			/** Added this condition to be able to handle an 8-byte LSB		**/
			/** integer.													**/
			/*---------------------------------------------------------------*/
			if (size == PDS_LONG_INT_BYTES)
			{
				for(byte=0; byte < PDS_LONG_INT_BYTES; ++byte) {
					if(byte == (PDS_LONG_INT_BYTES-1)) {
                        ftx_interpret_64_bits (sum, text[byte], byte, 0, 7, TRUE);
					}
					else {
						ftx_interpret_64_bits (sum, text[byte], byte, 0, 7, FALSE);
					}
				}

				result = stringvalfmt(sum, signed_value);
				strcpy(converted_text, result);
			}
			else
			/*----------------------------------------------------------------*/
			/** 10-13-02 MDC												 **/
			/** Added functionality to handle a 6-byte signed integer.		 **/
			/** It is basically a modified version of the above code that    **/
			/**	handles 8-byte integers.									 **/
			/*----------------------------------------------------------------*/
			if (size == PDS_SIX_INT_BYTES)
			{
				for (byte=0; byte < PDS_SIX_INT_BYTES; ++byte) {
					if(byte == (PDS_SIX_INT_BYTES-1)) {
                        ftx_interpret_64_bits(sum, text[byte], byte, 0, 7, TRUE);
					}
					else {
						ftx_interpret_64_bits(sum, text[byte], byte, 0, 7, FALSE);
					}
				}

				result = stringvalfmt(sum, signed_value);
				strcpy(converted_text, result);
			}
        }
        else
        {
            if (size == PDS_TINY_INT_BYTES)
            {
                u_num1 = ftx_interpret_bits (text[0], &pow2, 0, 7, FALSE);
                sprintf (converted_text, "%u", (short) u_num1);
            }
            else
            if (size == PDS_SHORT_INT_BYTES)
            {
                for (byte=0, pow2=1; byte < PDS_SHORT_INT_BYTES; ++byte)
		    u_num2 += ftx_interpret_bits(text[byte], &pow2, 0, 7,FALSE);
                sprintf (converted_text, "%u", u_num2);
            }
            else
            if (size == PDS_INT_BYTES) 
            {
                for (byte=0, pow2=1; byte < PDS_INT_BYTES; ++byte)
		    u_num4 += ftx_interpret_bits(text[byte], &pow2, 0, 7,FALSE);
                sprintf (converted_text, "%lu", u_num4);
            }
			else
			/*--------------------------------------------------------------*/
            /** 09.06.2002 MDC											   **/
			/** Added this condition to be able to handle an 8-byte LSB	   **/
			/** integer.												   **/
			/*--------------------------------------------------------------*/
			if (size == PDS_LONG_INT_BYTES)
			{
				for(byte=0; byte < PDS_LONG_INT_BYTES; ++byte)
					ftx_interpret_64_bits (sum, text[byte], byte, 0, 7, FALSE);

				result = stringvalfmt(sum, signed_value);
				strcpy(converted_text, result);
			}
			else
			/*----------------------------------------------------------------*/
			/** 10-13-02 MDC												 **/
			/** Added functionality to handle a 6-byte unsigned integer.	 **/
			/** It is basically a modified version of the above code that    **/
			/**	handles 8-byte integers.									 **/
			/*----------------------------------------------------------------*/
			if (size == PDS_SIX_INT_BYTES)
			{
				for (byte=0; byte < PDS_SIX_INT_BYTES; ++byte)
					ftx_interpret_64_bits(sum, text[byte], byte, 0, 7, FALSE);

				result = stringvalfmt(sum, signed_value);
				strcpy(converted_text, result);
			}

        }  /*  End:  "if (signed_value) ... else ..."  */

    }  /*  End:  "if ((size == PDS_TINY_INT_BYTES) || ... "  */

    return (converted_text);

}  /*  "ft_lsb_integer"  */



/*****/

unsigned char *ft_msb_binary (text, size)

unsigned char *text;
long size;

{
    BYTE_CONVERSION convert;
    unsigned char *converted_text = {NULL};
    unsigned char *t = {NULL};
    unsigned char *c = {NULL};
    long i;

    U_Malloc_String(converted_text, (int) (9*size) + 1);
    for (i=0,t=text,c=converted_text; i < size; ++i, ++t, ++c)
    {
        if (i > 0) 
        { 
            *c = ' '; ++c; 
        }
        convert.byte = *t;

        sprintf (c, "%d", (convert.bit.b0 != 0)); ++c;
        sprintf (c, "%d", (convert.bit.b1 != 0)); ++c;
        sprintf (c, "%d", (convert.bit.b2 != 0)); ++c;
        sprintf (c, "%d", (convert.bit.b3 != 0)); ++c;
        sprintf (c, "%d", (convert.bit.b4 != 0)); ++c;
        sprintf (c, "%d", (convert.bit.b5 != 0)); ++c;
        sprintf (c, "%d", (convert.bit.b6 != 0)); ++c;
        sprintf (c, "%d", (convert.bit.b7 != 0));
    }
    *c = EOS;

    return (converted_text);

}  /*  "ft_msb_binary"  */



/*****/

unsigned char *ft_msb_ieee_complex (text, size)

unsigned char *text;
long size;

{
    unsigned char *converted_text = {NULL};
    unsigned char *r_value = {NULL};
    unsigned char *c_value = {NULL};
    long c_size;

    c_size = (long) (size/2);

    r_value = ft_msb_ieee_real (text, c_size);
    c_value = ft_msb_ieee_real ((text + c_size), c_size);

    if ((r_value != NULL) && (c_value != NULL))
    { 
        U_Malloc_String(converted_text, 2*PDS_NUMBER_SIZE)
        sprintf(converted_text, "%s %s", r_value, c_value);
    }

    Lemme_Go(r_value)
    Lemme_Go(c_value)

    return (converted_text);

}  /*  "ft_msb_ieee_complex"  */


        
/*****/
         
unsigned char *ft_msb_ieee_real (text, size)

unsigned char *text;
long size;

{
    unsigned char *converted_text = {NULL};
    long pow2 = {1};
    unsigned long biased_exp = {0};
    long exponent = {0};
    double inv_pow2 = {0.5};
    double mantissa = {1.0};
    double f_num = {0.0};
    LOGICAL exponent_overflow = {FALSE};
	
    /*--------------------------------------------------------------------
     *
     *   For MSB IEEE 4-byte real numbers:
     *
     *      e0-bit  = lowest order bit of the exponent
     *      e1      = seven high order bits of the exponent
     *      m0 - m2 = order of bytes in mantissa (m0 = leftmost)
     *                (note that m0 has only seven bits)
     *      m-sign  = mantissa sign bit
     *      b0 - b3 = order of bytes in text string
     *
     *      m-sign
     *      |       e0-bit
     *      |       |
     *      |   e1  |   m0     m1      m2
     *      |v-----v|v-----vv------vv------v
     *      76543210765432107654321076543210
     *      |------||------||------||------|
     *         b0      b1      b2      b3
     *
     *      exponent bias = 127
     *
     *--------------------------------------------------------------------
     *
     *   For MSB IEEE 8-byte (double precision) real numbers:
     *
     *      e0      = four low order bits of the exponent
     *      e1      = seven high order bits of the exponent
     *      m0 - m6 = order of bytes in mantissa (m0 = leftmost)
     *                (note that m0 has only four bits)
     *      m-sign  = mantissa sign bit
     *      b0 - b7 = order of bytes in text string
     *    
     *      exponent bias = 1023
     *
     *    m-sign
     *    |
     *    |   e1   e0  m0    m1      m2      m3      m4      m5      m6
     *    |v-----vv--vv--vv------vv------vv------vv------vv------vv------v 
     *    7654321076543210765432107654321076543210765432107654321076543210
     *    |------||------||------||------||------||------||------||------|
     *       b0      b1      b2      b3      b4      b5      b6      b7
     *
     *--------------------------------------------------------------------
     *
     *   For MSB IEEE 10-byte (temporary) real numbers:
     *
     *      e0      = eight low order bits of the exponent
     *      e1      = seven high order bits of the exponent
     *      m0 - m7 = order of bytes in mantissa (m0 = leftmost)
     *                (note that m0 has only seven bits)
     *      m-sign  = mantissa sign bit
     *      int-bit = integer bit ("1." part) of mantissa
     *      b0 - b9 = order of bytes in text string
     *    
     *      exponent bias = 16383
     *
     *    m-sign          int-bit
     *    |               |
     *    |   e1     e0   |  m0      m1      m2
     *    |v-----vv------v|v-----vv------vv------v
     *    7654321076543210765432107654321076543210
     *    |------||------||------||------||------|
     *       b0      b1      b2      b3      b4
     *
     *       m3      m4      m5      m6      m7
     *    v------vv------vv------vv------vv------v
     *    7654321076543210765432107654321076543210
     *    |------||------||------||------||------|
     *       b5      b6      b7      b8      b9
     *
     *--------------------------------------------------------------------
     *
     *   These representations all follow the format:
     *
     *       1.(mantissa) x 2**(exponent - bias)
     *
     *   with the "1." part implicit (except for the 10-byte temp real, 
     *   in which the "1." part is actually stored in the third byte (b2)),
     *
     *   In all cases, the exponent is stored as an unsigned, biased
     *   integer.
     *
     *--------------------------------------------------------------------
     */

    if ((size == PDS_IEEE_REAL_BYTES) ||
          (size == PDS_IEEE_DOUBLE_BYTES) || (size == PDS_IEEE_TEMP_BYTES))
    {
        if (size == PDS_IEEE_REAL_BYTES)
        {
            pow2 = 1;
            biased_exp += ftx_interpret_bits (text[1], &pow2, 7, 7, FALSE);
            biased_exp += ftx_interpret_bits (text[0], &pow2, 0, 6, FALSE);
            exponent = biased_exp - 127;

            if (exponent > PDS_MAX_EXPONENT)
                exponent_overflow = TRUE;
            else
            {
                inv_pow2 = 0.5;
                mantissa += ftx_interpret_fraction (text[1], &inv_pow2, 6, 0);
                mantissa += ftx_interpret_fraction (text[2], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[3], &inv_pow2, 7, 0);
            }
        }
        else
        if (size == PDS_IEEE_DOUBLE_BYTES)
        {
            pow2 = 1;
            biased_exp += ftx_interpret_bits (text[1], &pow2, 4, 7, FALSE);
            biased_exp += ftx_interpret_bits (text[0], &pow2, 0, 6, FALSE);
            exponent = biased_exp - 1023;

           if (exponent > PDS_MAX_EXPONENT)
				exponent_overflow = TRUE;
            else
            {
                inv_pow2 = 0.5;
                mantissa += ftx_interpret_fraction (text[1], &inv_pow2, 3, 0);
                mantissa += ftx_interpret_fraction (text[2], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[3], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[4], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[5], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[6], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[7], &inv_pow2, 7, 0);
            }
        }
        else
        if (size == PDS_IEEE_TEMP_BYTES)
        {
            pow2 = 1;
            biased_exp += ftx_interpret_bits (text[1], &pow2, 0, 7, FALSE);
            biased_exp += ftx_interpret_bits (text[0], &pow2, 0, 6, FALSE);
            exponent = biased_exp - 16383;

            if (exponent > PDS_MAX_EXPONENT)
                exponent_overflow = TRUE;
            else
            {
                inv_pow2 = 0.5;
                mantissa += ftx_interpret_fraction (text[2], &inv_pow2, 6, 0);
                mantissa += ftx_interpret_fraction (text[3], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[4], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[5], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[6], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[7], &inv_pow2, 7, 0);
#ifdef VAX
                mantissa += ftx_interpret_fraction (text[8], &inv_pow2, 7, 0);
#else
                mantissa += ftx_interpret_fraction (text[8], &inv_pow2, 7, 3);
#endif
            }
        }
		
		if (! exponent_overflow)
        {
			f_num = (text[0]&128) ? - ldexp (mantissa, (int) exponent) :
				      ldexp (mantissa, (int) exponent);
			
		    U_Malloc_String(converted_text, PDS_NUMBER_SIZE);
			
		    /*------------------------------------------------------------*/
			/** 08-09-02  MDC											 **/	
			/** Added a check to make sure that the value does not		 **/ 
			/** exceed the amount of space allocated to the				 **/
			/** converted_text variable f_num will go into. If it does,	 **/
			/** display an error message.								 **/
			/*------------------------------------------------------------*/

			if( (f_num > (pow(2.0,PDS_NUMBER_SIZE)-1.0)) || 
									(f_num < (-pow(2.0,PDS_NUMBER_SIZE)+1.0)) )
				converted_text = 0;
			else
			{ 
				sprintf (converted_text, "%20.10lf", f_num);
			}  
		}
	
    }  /*  End:  "if ((size == PDS_IEEE_REAL_BYTES) || ..."  */

    return (converted_text);

}  /*  "ft_msb_ieee_real"  */


        
/*****/

/*********************************************************************/
/* Change History                                                    */
/*    08-21-2003   MDC    Corrected the logic to properly calculate  */
/*                        an unformatted value.                      */
/*********************************************************************/

unsigned char *ft_msb_integer (text, size, signed_value)

unsigned char *text;
long size;
LOGICAL signed_value;

{
    unsigned char *converted_text = {NULL};
    
	long byte;
    char num1 = {0};
    short num2 = {0};
    long num4 = {0};
    unsigned char u_num1 = {0};
    unsigned short u_num2 = {0};
    unsigned long u_num4 = {0};
    long pow2 = {1};
	
	/*-----------------------------------------------------------------*/
    /** 08.29.02 MDC												  **/
	/** Added this variable to hold the result of an 8-byte integer.  **/
	/**																  **/
	/** 10-13-02 MDC												  **/
	/** This variable will also hold the result of a 6-byte integer.  **/
	/*-----------------------------------------------------------------*/
	unsigned char *result = NULL;
	
	/*------------------------------------------------------------------*/
	/** 09.06.02 MDC												   **/
	/** Added this array to store the decimal representation of the	   **/
	/** 8-byte integer. One digit is stored per memory space with the  **/
	/** one's digit stored at the end of this array.				   **/
	/*------------------------------------------------------------------*/
	short sum[MAX_ARRAY_SIZE];
	long x = 0;
	
	/* Initialize the array to all zeros. */
	for(x=0; x < MAX_ARRAY_SIZE; x++)
		sum[x] = 0;

	/*--------------------------------------------------------------------
	 *  10-13-02 MDC - Added functionality to handle 6 byte integers
	 *
	 * For 6-byte integers:
	 *
	 *      i0 - i5 = order of bytes in integer (i0 = lowest order)
     *      i-sign  = integer sign bit
     *      b0 - b5 = order of bytes in text string
     *
     *      i-sign
     *      |
     *      |   i5     i4      i3      i2	   i1      i0      
     *      |v-----vv------vv------vv------vv------vv------v
     *      765432107654321076543210765432107654321076543210
     *      |------||------||------||------||------||------|
     *         b0      b1      b2      b3      b4      b5      
     *
     *--------------------------------------------------------------------
     *	08.29.2002 MDC - Added functionality to handle 8 byte integers
	 *
	 *  For 8-byte integers:
     *      
     *      i0 - i7 = order of bytes in integer (i0 = lowest order)
     *      i-sign  = integer sign bit
     *      b0 - b7 = order of bytes in text string
     *
     *      i-sign
     *      |
     *      |   i7     i6      i5      i4	   i3      i2      i1      i0
     *      |v-----vv------vv------vv------vv------vv------vv------vv------v
     *      7654321076543210765432107654321076543210765432107654321076543210
     *      |------||------||------||------||------||------||------||------|
     *         b0      b1      b2      b3      b4      b5      b6      b7
     *
	 *--------------------------------------------------------------------
	 *
     *  For 4-byte integers:
     *      
     *      i0 - i3 = order of bytes in integer (i0 = lowest order)
     *      i-sign  = integer sign bit
     *      b0 - b3 = order of bytes in text string
     *
     *      i-sign
     *      |
     *      |   i3     i2      i1      i0
     *      |v-----vv------vv------vv------v
     *      76543210765432107654321076543210
     *      |------||------||------||------|
     *         b0      b1      b2      b3
     *
     *--------------------------------------------------------------------
     *
     *  For 2-byte integers:
     *      
     *      i-sign
     *      |
     *      |  i1      i0
     *      |v-----vv------v
     *      7654321076543210
     *      |------||------|
     *         b0      b1   
     *
     *--------------------------------------------------------------------
     *
     *  For 1-byte integers:
     *      
     *      i-sign
     *      |
     *      |  i0
     *      |v-----v
     *      76543210
     *      |------|
     *         b0   
     *
     *--------------------------------------------------------------------
     *
     *  All negative signed values are assumed to be twos-complement.
     *
     *--------------------------------------------------------------------
     */

	/*-------------------------------------------------------------------*/
    /** 10-13-02 MDC													**/
	/** Added a check to look for a 6-byte unsigned or signed integer.  **/
	/*-------------------------------------------------------------------*/

    if ((size == PDS_TINY_INT_BYTES) || (size == PDS_SHORT_INT_BYTES) || 
		(size == PDS_INT_BYTES) || (size == PDS_LONG_INT_BYTES) || (size == PDS_SIX_INT_BYTES) )
    {        
        U_Malloc_String(converted_text, PDS_NUMBER_SIZE);
		
		/*--------------------------------------------------------------------*/
		/** 09.04.2002 MDC													 **/ 
		/** Changed the LOGICAL parameter going into the					 **/
		/** ftx_interpret_bits routine to "TRUE" instead of "FALSE" if a	 **/
		/** signed integer is detected.										 **/
		/*--------------------------------------------------------------------*/
		
		/*--------------------------------------------------------------------*/
		/* 08-21-2003 MDC                                                     */
		/* The most significant byte (text[0] in this case) is the only one   */
		/* where the most significant bit is the signed bit. So, only pass in */
		/* "TRUE" when interpreting the most significant byte to prevent      */
		/* miscalculation of the unformatted value.                           */
		/*--------------------------------------------------------------------*/
        if (signed_value)
        {
            if (size == PDS_TINY_INT_BYTES)
            {
                num1 = ftx_interpret_bits (text[0], &pow2, 0, 7, TRUE);
                sprintf (converted_text, "%d", (short) num1);
            }
            else
            if (size == PDS_SHORT_INT_BYTES)
            {
				for (byte=size-1, pow2=1; byte >= 0; --byte) {
					if(byte == 0) {
                        num2 += ftx_interpret_bits (text[byte], &pow2, 0, 7, TRUE);
					}
					else {
						num2 += ftx_interpret_bits (text[byte], &pow2, 0, 7, FALSE);
					}
				}
                
				sprintf (converted_text, "%d", num2);
            }
            else
            if (size == PDS_INT_BYTES) 
            {
				for (byte=size-1, pow2=1; byte >= 0; --byte) {
					if(byte == 0) {
						num4 += ftx_interpret_bits(text[byte], &pow2, 0, 7, TRUE);
					}
					else {
                        num4 += ftx_interpret_bits (text[byte], &pow2, 0, 7, FALSE);
					}
				}
                
				sprintf (converted_text, "%ld", num4);
            }
			else
			if (size == PDS_LONG_INT_BYTES)
			{
				for(byte=size-1; byte >= 0; --byte) {
					if(byte == 0) {
						ftx_interpret_64_bits(sum, text[byte], (7-byte), 0, 7, TRUE);
					}
					else {
						ftx_interpret_64_bits(sum, text[byte], (7-byte), 0, 7, FALSE);
					}
				}
				
				result = stringvalfmt(sum, signed_value); 
				strcpy(converted_text, result);
			}
			else
			/*----------------------------------------------------------------*/
			/** 10-13-02 MDC												 **/
			/** Added functionality to handle a 6-byte signed integer.		 **/
			/** It is basically a modified version of the above code that    **/
			/**	handles 8-byte integers.									 **/
			/*----------------------------------------------------------------*/
			if (size == PDS_SIX_INT_BYTES)
			{
				for (byte=size-1; byte >= 0; --byte) {
					if(byte == 0) {
                        ftx_interpret_64_bits(sum, text[byte], (5-byte), 0, 7, TRUE);
					}
					else {
						ftx_interpret_64_bits(sum, text[byte], (5-byte), 0, 7, FALSE);
					}
				}

				result = stringvalfmt(sum, signed_value);
				strcpy(converted_text, result);
			}
        }
        else
        {
            if (size == PDS_TINY_INT_BYTES)
            {
                u_num1 = ftx_interpret_bits (text[0], &pow2, 0, 7, FALSE);
				sprintf (converted_text, "%u", (short) u_num1);
			}
			else
			if (size == PDS_SHORT_INT_BYTES)
			{
				for (byte=size-1, pow2=1; byte >= 0; --byte)
					u_num2 += ftx_interpret_bits (text[byte], &pow2, 0,7,FALSE);
				sprintf (converted_text, "%u", u_num2);
			}
			else
			if (size == PDS_INT_BYTES)
			{
				for (byte=size-1, pow2=1; byte >= 0; --byte)
					u_num4 += ftx_interpret_bits (text[byte], &pow2, 0,7,FALSE);
				sprintf (converted_text, "%lu", u_num4);
			}
			else
			/* 08.29.02 MDC - Added functionality to handle an 8 byte integer. */
			if (size == PDS_LONG_INT_BYTES)
			{	
				for (byte=size-1; byte >= 0; --byte)
					ftx_interpret_64_bits(sum, text[byte], (7-byte), 0, 7, FALSE); 
				
				result = stringvalfmt(sum, signed_value);
				strcpy(converted_text, result);
			}
			else
			/*----------------------------------------------------------------*/
			/** 10-13-02 MDC												 **/
			/** Added functionality to handle a 6-byte unsigned integer.	 **/
			/** It is basically a modified version of the above code that    **/
			/**	handles 8-byte integers.									 **/
			/*----------------------------------------------------------------*/
			if (size == PDS_SIX_INT_BYTES)
			{
				for (byte=size-1; byte >= 0; --byte)
					ftx_interpret_64_bits(sum, text[byte], (5-byte), 0, 7, FALSE);

				result = stringvalfmt(sum, signed_value);
				strcpy(converted_text, result);
			}

		}  /*  End:  "if (signed_value) ... else ..."  */

    }  /*  End:  "if ((size == PDS_TINY_INT_BYTES) || ..."  */

    return (converted_text);

}  /*  "ft_msb_integer"  */



/*****/

unsigned char *ft_octal (text, size)

unsigned char *text;
long size;

{
    unsigned char *converted_text = {NULL};
    unsigned char *t = {NULL};
    unsigned char *c = {NULL};
    long i;

    U_Malloc_String(converted_text, (int) (2*(size + (int) (size/4) + 1)));
    for (i=0,t=text,c=converted_text; i < size; ++i, ++t, c += 2)
    {
        if (((i % 4) == 0) && (i > 0))
        {
            *c = ' ';
            ++c;
        }
        if (*t < 8)
            sprintf (c, "0%o", *t);
        else
            sprintf (c, "%o", *t);
    }
    *c = EOS;

    return (converted_text);

}  /*  "ft_octal"  */



/*****/

unsigned char *ft_vax_complex (text, size, vaxg_type)

unsigned char *text;
long size;
LOGICAL vaxg_type;

{
    unsigned char *converted_text = {NULL};
    unsigned char *r_value = {NULL};
    unsigned char *c_value = {NULL};
    long c_size;

    c_size = (long) (size/2);

    r_value = ft_vax_real (text, c_size, vaxg_type);
    c_value = ft_vax_real ((text + c_size), c_size, vaxg_type);

    if ((r_value != NULL) && (c_value != NULL))
    { 
        U_Malloc_String(converted_text, 2*PDS_NUMBER_SIZE)
        sprintf(converted_text, "%s %s", r_value, c_value);
    }

    Lemme_Go(r_value)
    Lemme_Go(c_value)

    return (converted_text);

}  /*  "ft_vax_complex"  */


        
/*****/

unsigned char *ft_vax_real (text, size, vaxg_type)

unsigned char *text;
long size;
LOGICAL vaxg_type;

{
    unsigned char *converted_text = {NULL};
    long pow2 = {1};
    unsigned long biased_exp = {0};
    long exponent = {0};
    double inv_pow2 = {0.5};
    double mantissa = {1.0};
    double f_num = {0.0};
    LOGICAL exponent_overflow = {FALSE};

    /*--------------------------------------------------------------------
     *
     *   For VAX F-type 4-byte real numbers:
     *
     *      e0-bit  = lowest order bit of the exponent
     *      e1      = seven high order bits of the exponent
     *      m0 - m2 = order of bytes in mantissa (m0 = leftmost)
     *                (note that m0 has only seven bits)
     *      m-sign  = mantissa sign bit
     *      b0 - b3 = order of bytes in text string
     *
     *      e0-bit
     *      |       m-sign
     *      |       |
     *      |   m0  |   e1     m2      m1
     *      |v-----v|v-----vv------vv------v
     *      76543210765432107654321076543210
     *      |------||------||------||------|
     *         b0      b1      b2      b3
     *
     *      exponent bias = 129
     *
     *   To interpret this as a more typical floating point representation,
     *   (e.g., sign-exponent-mantissa form) you must rearrange the
     *   bytes in this order: b1 b0 b3 b2
     *
     *--------------------------------------------------------------------
     *
     *   For VAX D-type 8-byte real numbers:
     *
     *      e0-bit  = lowest order bit of the exponent
     *      e1      = seven high order bits of the exponent
     *      m0 - m6 = order of bytes in mantissa (m0 = leftmost)
     *                (note that m0 has only seven bits)
     *      m-sign  = mantissa sign bit
     *      b0 - b7 = order of bytes in text string
     *    
     *      exponent bias = 129
     *
     *    e0-bit
     *    |       m-sign
     *    |       |
     *    |  m0   |  e1      m2      m1      m4      m3      m6      m5 
     *    |v-----v|v-----vv------vv------vv------vv------vv------vv------v 
     *    7654321076543210765432107654321076543210765432107654321076543210
     *    |------||------||------||------||------||------||------||------|
     *       b0      b1      b2      b3      b4      b5      b6      b7
     *
     *   To interpret this as a more typical floating point representation,
     *   (e.g., sign-exponent-mantissa form) you must rearrange the
     *   bytes in this order: b1 b0 b3 b2 b5 b4 b7 b6
     *
     *--------------------------------------------------------------------
     *
     *   For VAX G-type 8-byte real numbers:
     *
     *      e0      = four lowest order bits of the exponent
     *      e1      = seven high order bits of the exponent
     *      m0 - m6 = order of bytes in mantissa (m0 = leftmost)
     *                (note that m0 has only four bits)
     *      m-sign  = mantissa sign bit
     *      b0 - b7 = order of bytes in text string
     *    
     *      exponent bias = 1025
     *
     *            m-sign
     *            |
     *     e0  m0 |  e1      m2      m1      m4      m3      m6      m5 
     *    v--vv--v|v-----vv------vv------vv------vv------vv------vv------v 
     *    7654321076543210765432107654321076543210765432107654321076543210
     *    |------||------||------||------||------||------||------||------|
     *       b0      b1      b2      b3      b4      b5      b6      b7
     *
     *   To interpret this as a more typical floating point representation,
     *   (e.g., sign-exponent-mantissa form) you must rearrange the
     *   bytes in this order: b1 b0 b3 b2 b5 b4 b7 b6
     *
     *--------------------------------------------------------------------
     *
     *   For VAX H-type 16-byte real numbers:
     *
     *      e0       = eight lowest order bits of the exponent
     *      e1       = seven high order bits of the exponent
     *      m0 - m13 = order of bytes in mantissa (m0 = leftmost)
     *      m-sign   = mantissa sign bit
     *      b0 - b15 = order of bytes in text string
     *    
     *      exponent bias = 16385
     *
     *            m-sign
     *            |
     *       e0   |  e1      m1      m0      m3      m2      m5      m4
     *    v------v|v-----vv------vv------vv------vv------vv------vv------v 
     *    7654321076543210765432107654321076543210765432107654321076543210
     *    |------||------||------||------||------||------||------||------|
     *       b0      b1      b2      b3      b4      b5      b6      b7
     *
     *       m7      m6      m9      m8      m11     m10     m13     m12
     *    v------vv------vv------vv------vv------vv------vv------vv------v 
     *    7654321076543210765432107654321076543210765432107654321076543210
     *    |------||------||------||------||------||------||------||------|
     *       b8      b9      b10     b11     b12     b13     b14     b15
     *
     *   To interpret this as a more typical floating point representation,
     *   (e.g., sign-exponent-mantissa form) you must rearrange the
     *   bytes in this order: 
     *
     *          b1 b0 b3 b2 b5 b4 b7 b6 b9 b8 b11 b10 b13 b12 b15 b14
     *
     *--------------------------------------------------------------------
     *
     *   These representations all follow the format:
     *
     *       1.(mantissa) x 2**(exponent - bias)
     *
     *   with the "1." part implicit.
     *
     *   In all cases, the exponent is stored as an unsigned, biased
     *   integer.
     *
     *--------------------------------------------------------------------
     */

    if ((size == PDS_VAXF_REAL_BYTES) ||
          (size == PDS_VAXD_REAL_BYTES) || (size == PDS_VAXH_REAL_BYTES))
    {
        if (size == PDS_VAXF_REAL_BYTES) 
        {
            pow2 = 1;
            biased_exp += ftx_interpret_bits (text[0], &pow2, 7, 7, FALSE);
            biased_exp += ftx_interpret_bits (text[1], &pow2, 0, 6, FALSE);
            exponent = biased_exp - 129;

            if (exponent > PDS_MAX_EXPONENT)
                exponent_overflow = TRUE;
            else
            {
                inv_pow2 = 0.5;
                mantissa += ftx_interpret_fraction (text[0], &inv_pow2, 6, 0);
                mantissa += ftx_interpret_fraction (text[3], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[2], &inv_pow2, 7, 0);
            }
        }
        else
        if (! vaxg_type && (size == PDS_VAXD_REAL_BYTES))
        {
            pow2 = 1;
            biased_exp += ftx_interpret_bits (text[0], &pow2, 7, 7, FALSE);
            biased_exp += ftx_interpret_bits (text[1], &pow2, 0, 6, FALSE);
            exponent = biased_exp - 129;

            if (exponent > PDS_MAX_EXPONENT)
                exponent_overflow = TRUE;
            else
            {
                inv_pow2 = 0.5;
                mantissa += ftx_interpret_fraction (text[0], &inv_pow2, 6, 0);
                mantissa += ftx_interpret_fraction (text[3], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[2], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[5], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[4], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[7], &inv_pow2, 7, 0);
#ifdef VAX
                mantissa += ftx_interpret_fraction (text[6], &inv_pow2, 7, 0);
#else
                mantissa += ftx_interpret_fraction (text[6], &inv_pow2, 7, 3);
#endif
            }
        }
        else
        if (vaxg_type && (size == PDS_VAXD_REAL_BYTES))
        {
            pow2 = 1;
            biased_exp += ftx_interpret_bits (text[0], &pow2, 4, 7, FALSE);
            biased_exp += ftx_interpret_bits (text[1], &pow2, 0, 6, FALSE);
            exponent = biased_exp - 1025;

            if (exponent > PDS_MAX_EXPONENT)
                exponent_overflow = TRUE;
            else
            {
                inv_pow2 = 0.5; 
                mantissa += ftx_interpret_fraction (text[0], &inv_pow2, 3, 0);
                mantissa += ftx_interpret_fraction (text[3], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[2], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[5], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[4], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[7], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[6], &inv_pow2, 7, 0);
            }
        }
        else
        if (size == PDS_VAXH_REAL_BYTES)
        {
            pow2 = 1;
            biased_exp += ftx_interpret_bits (text[0], &pow2, 0, 7, FALSE);
            biased_exp += ftx_interpret_bits (text[1], &pow2, 0, 6, FALSE);
            exponent = biased_exp - 16385;

            if (exponent > PDS_MAX_EXPONENT)
                exponent_overflow = TRUE;
            else
            {
                inv_pow2 = 0.5;
                mantissa += ftx_interpret_fraction (text[3], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[2], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[5], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[4], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[7], &inv_pow2, 7, 0);
                mantissa += ftx_interpret_fraction (text[6], &inv_pow2, 7, 0);
#ifdef VAX
                mantissa += ftx_interpret_fraction (text[9], &inv_pow2, 7, 1);
#else
                mantissa += ftx_interpret_fraction (text[9], &inv_pow2, 7, 4);
#endif
            }
        }

        if (! exponent_overflow)
        {
	    f_num = (text[1]&128) ? - ldexp (mantissa, (int) exponent) :
				      ldexp (mantissa, (int) exponent);
            U_Malloc_String(converted_text, PDS_NUMBER_SIZE);
            sprintf (converted_text, "%20.10lf", f_num);
        }

    }  /*  End:  "if ((size == PDS_VAXF_REAL_BYTES) || ..."  */

    return (converted_text);

}  /*  "ft_vax_real"  */


        
/*****/

static long ftx_interpret_bits (unsigned char byte, long *pow2, long start_bit,
								                  long stop_bit, LOGICAL signed_value)
{
    unsigned char mask;
    long value = 0;
    long bit = 0;

    /*-----------------------------------------------------------------
     *
     *   Here's how start_bit and stop_bit relate in a sample byte:
     *
     *                     76543210
     *                       ^    ^
     *                       |    |
     *                       |    start_bit
     *                       |
     *                       stop_bit
     *
     *   Since each bit represents a power of two, and the order of the 
     *   bits progresses from right to left, start_bit will always be 
     *   to the right of stop_bit.  If these six bits represent a
     *   signed number, then stop_bit is the sign bit.
     *
     *-----------------------------------------------------------------
     */

    for (bit=0, mask=1; ((bit <= 7) && (bit <= stop_bit)); ++bit)
    {
        if (bit >= start_bit)
        {
            if ((mask & byte) == mask)
            {                      
                if (signed_value && (bit == stop_bit))
				{
					/*---------------------------------------------------*/
					/** 09.04.2002 MDC									**/
					/**													**/
					/** Corrected the computation so that the correct	**/
					/** value will be calculated for a signed integer.  **/
					/*---------------------------------------------------*/
                    /* value -= 2*value; */
					value -= (*pow2);
				}
                else
                    value += (*pow2);
            }

            *pow2 = 2 * (*pow2);

        }  /*  End:  "if (bit >= start_bit) ..."  */

        mask = 2*mask;

    }  /*  End:  "for (bit=0; ..."  */

    return (value);

}  /*  "ftx_interpret_bits"  */

/*---------------------------------------------------------------------*/
/** 08.29.2002 MDC													  **/
/** Added this function when an integer greater than 4-bytes is		  **/ 
/** encountered. Currently it is used when a 6 or 8-byte integer is   **/  
/**	found. It is basically a copy of the above function, but with a	  **/
/** different return type.											  **/
/**																	  **/
/**	Inputs:															  **/
/**		1) sum[]: An array to hold the integer. The value can be read **/
/**				  by reading the array from lowest index to highest   **/
/**				  index. In other words, sum[19] is the one's place.  **/
/**		2) byte: The byte to interpret.								  **/
/**		3) offset: The location of the byte with reference to the	  **/
/**				   whole value being interpreted. For example, offset **/
/**				   could be 2, meaning the 2nd byte in the value.	  **/
/**				   This is used mainly for calculating the 2^N value  **/
/**				   of a byte in the 2-D array, which holds the values **/
/**				   between 2^0 and 2^64.							  **/
/**		4) start_bit: The location of the bit to start within a byte. **/
/**		5) stop_bit: The location of the bit to stop at within a byte.**/
/**		6) signed_value: TRUE if the integer is signed, FALSE if the  **/
/**						 integer is unsigned.						  **/
/*---------------------------------------------------------------------*/

void ftx_interpret_64_bits (short sum[], unsigned char byte, long offset, 
								long start_bit, long stop_bit, LOGICAL signed_value)
{
    unsigned char mask;
    long bit = 0;
	short i = 0;
		    
    /*-----------------------------------------------------------------
     *
     *   Here's how start_bit and stop_bit relate in a sample byte:
     *
     *                     76543210
     *                     ^      ^
     *                     |      |
     *                     |      start_bit
     *                     |  
     *                     stop_bit
     *
     *   Since each bit represents a power of two, and the order of the 
     *   bits progresses from right to left, start_bit will always be 
     *   to the right of stop_bit.  If these six bits represent a
     *   signed number, then stop_bit is the sign bit.
     *
     *-----------------------------------------------------------------
     */
	for (bit=0, mask=1; ((bit <= 7) && (bit <= stop_bit)); ++bit) 
	{
		if (bit >= start_bit)
		{
			if ((mask & byte) == mask)
				calc_value(sum, offset, bit, stop_bit, signed_value);
			
		}  /*  End:  "if (bit >= start_bit) ..."  */

		mask = 2*mask;

	}  /*  End:  "for (bit=0; ..."  */
		
}  /*  "ftx_interpret_64_bits"  */


/*---------------------------------------------------------------------------------*/
/** 09.06.02 MDC																  **/
/** This routine will create a string variable based on the sum array. It will	  **/ 
/** take each digit and concatenate it to the end of the string until the end of  **/
/** the array. This routine has been created since currently, there is no ANSI C  **/ 
/** standard data type to handle an 8-byte int.									  **/
/**																				  **/
/** 10-13-02 MDC																  **/
/** Added a check to make the routine return a "0" if the sum array is 0. tbtool  **/
/** crashes if this check is not made.											  **/
/*---------------------------------------------------------------------------------*/
unsigned char *stringvalfmt(short sum[], LOGICAL signed_value)
{
	short n = 0, offset = 0;
	unsigned char *digit = NULL;
	unsigned char *valptr = NULL;
	unsigned char value[MAX_ARRAY_SIZE];
	
	for(n=0; n<MAX_ARRAY_SIZE; n++)
		value[n] = 0;
    
	/*----------------------------------------------------------------------------*/
	/** Move to the next digit to the right in the array if there are leading	 **/ 
	/** zeros.																	 **/
	/**																			 **/
	/** 10-13-02 Added by MDC													 **/
	/** If the offset equals 20, that means that the entire sum array is 0.		 **/
	/** Return a "0" string if this is the case.								 **/
	/*----------------------------------------------------------------------------*/
	while( (sum[offset] == 0) && (offset != MAX_ARRAY_SIZE) )
	{
		offset++;
		if(offset == MAX_ARRAY_SIZE)
			return "0";
	}


	/* Must allocate 2 bytes since it is a number + NULL character that is going 
	   into this variable.*/
	digit = malloc(sizeof(char)+1);

	if(digit == NULL)
	{
		printf("ERROR: Could not allocate enough memory for the variable digit.\n\nEnding program.........\n");
		exit(0);
	}
	
	/* If this is a signed number, make sure a '-' sign goes in front of the string variable. */
	if(signed_value == TRUE)
		strcpy(value, "-");

	sprintf(digit, "%d", sum[offset++]);
	strcpy(value, digit);
    
	while(offset != MAX_ARRAY_SIZE)
	{
		sprintf(digit, "%d", sum[offset++]);
		strcat(value, digit);
	}

	valptr = value;
	free(digit);
	return(valptr);
}

/*-----------------------------------------------------------------------*/
/** 09.06.2002 MDC														**/ 
/** This routine was added to calculate the decimal representation of an**/
/** 8-byte integer. The value is stored in such a way that each digit is**/
/** placed in the sum array. The one's place is located at the very end **/
/** of the sum array. As soon as an ANSI C standard of handling an		**/ 
/** 8-byte integer is developed, this routine can be obsoleted.			**/
/**																		**/
/** Inputs:																**/
/**		1) sum[]: An array of 20 short types in which a 6 or 8 byte     **/
/**				  value can be held. The value can be read by reading   **/
/**				  the array form left to right, where sum[0] holds the  **/
/**				  highest place integer and sum[19] holds the one's     **/
/**				  place.												**/
/**		2) bytenum: The location of the byte. For example, if bytenum=1,**/
/**					it references to bits 0-7, bytenum=2 references to  **/
/**					bits 8-15, and so on. This is used in calculating   **/
/**				    what value to pull out in the 2-D array which holds **/
/**					the values between 2^0 and 2^64.					**/
/**		3) bit: the bit number that you are on within a byte.			**/
/**		4) stop_bit: the location of the stop_bit within a byte.		**/
/**		5) sign: Whether a value is signed or unsigned.					**/
/**																		**/
/** Returns:															**/
/**		The bit interpretation will be stored in memory in the sum		**/
/**		array, so you don't need to return anything.					**/
/*-----------------------------------------------------------------------*/

void calc_value(short sum[], short bytenum, 
								short bit, short stop_bit, LOGICAL sign)
{
	short n, carry = 0, borrow = 0, check = 0;
	char *tempptr = NULL;

	if (sign && (bit == stop_bit))
	{
		/*------------------------------------------------------------------*/
		/** If it is a signed value, begin from the one's place which is at**/
		/** the end of the array. Begin subtracting the current value in   **/
		/** the sum array with the appropriate value obtained from the 2-D **/
		/** power array.												   **/
		/*------------------------------------------------------------------*/
		for(n = MAX_ARRAY_SIZE - 1; n > -1; --n)
		{
			while( (sum[n] == 0) && (powers[(8*bytenum)+bit][n] == 0) 
					&& (borrow ==0) && (n!=0) )
				n--;

			check = powers[(8*bytenum)+bit][n] - sum[n] + borrow;

			/*-------------------------------------------------------------*/
			/** If the check ends up being a negative number, add 10 to	  **/ 
			/** this value and store it in the corresponding space in the **/
			/** array. borrow will be set to -1 to account for borrowing  **/
			/** from the digit to the left.								  **/
			/*-------------------------------------------------------------*/
			if(check < 0)
			{
				sum[n] = check + 10;
				borrow = -1;
			}
			else
			{
				sum[n] = check;
				borrow = 0;
			}
		}
	}
	else
	{ 
		/* Start from the one's place (end of the sum array). */
		for(n = MAX_ARRAY_SIZE - 1; n > -1; --n)
		{
			/*------------------------------------------------------------*/
			/** If all these values are '0', then you can move on to the **/
			/** next set of digits. This will help execute this routine  **/
			/**	alot faster.											 **/
			/*------------------------------------------------------------*/

			while( (sum[n] == 0) && (powers[(8*bytenum)+bit][n] == 0) 
						&& (carry == 0) && (n != 0) )
				n--;
			
			/*-----------------------------------------------------------*/
			/** Obtain the corresponding value in the 2-D array and see **/
			/** if the sum of this and the corresponding digit in the	**/
			/** current running total is more than 10.					**/
			/*-----------------------------------------------------------*/
  			check = sum[n] + powers[(8*bytenum)+bit][n] + carry;
			
			/*-----------------------------------------------------------*/
            /** If the sum is 10 or more, place the digit in the one's  **/
			/** position and bring a carry over to the next place to	**/ 
			/** the left.												**/
			/*-----------------------------------------------------------*/
			if(check >= 10)
			{
				/*--------------------------------------------------------*/
				/** must allocate a total of 3 bytes since the max number**/ 
				/** you will obtain in this routine will be 2 digits plus**/
				/** the NULL character at the end of the string.		 **/
				/*--------------------------------------------------------*/
				tempptr = malloc(sizeof(char) + 2);
				if(tempptr == NULL)
				{
					printf("ERROR: Not Enough memory to allocate space for tempptr variable\n");
					exit(0);
				}
				sprintf(tempptr, "%d", check);
				/*--------------------------------------------------------*/
				/** Move the pointer over so that you now point to the	 **/
				/**	one's place of the number.							 **/
				/*--------------------------------------------------------*/
				tempptr++;
				sum[n] = atoi(tempptr);
				carry = 1;
				/*--------------------------------------------------------*/
				/** Move the pointer back so that you can correctly free **/
				/** the allocated memory.								 **/
				/*--------------------------------------------------------*/
				tempptr--;
				free(tempptr);
				tempptr = NULL;
			}
			else
			{
				sum[n] = check;
				carry = 0;
			}
			
			if( (n == 0) && (carry == 1) )
			{
				printf("ERROR: Overflow occurred. Value is too big.\n\n");
				printf("Ending program...................\n");
				exit(0);
			}
		} /* End "for(n = MAX_ARRAY_SIZE - 1;..." */
	} /* End "else" */ 
	
} /* End calc_value */

/*****/

static double ftx_interpret_fraction (unsigned char byte, double *inverse_pow2,
									         long start_bit, long stop_bit)

/*unsigned char byte;
double *inverse_pow2;
long start_bit;
long stop_bit;
*/
{
    unsigned char mask;
    long bit;
    double value = {0.0};

    /*-----------------------------------------------------------------
     *
     *   Here's how start_bit and stop_bit relate in a sample byte:
     *
     *                     76543210
     *                      ^    ^
     *                      |    |
     *                      |    stop_bit
     *                      |
     *                      start_bit
     *
     *   Since each bit represents a fractional power of two, and the
     *   order of the bits progresses from left to right, start_bit
     *   will always be to the left of stop_bit.
     *
     *-----------------------------------------------------------------
     */

    for (bit=7, mask=128; ((bit >= 0) && (bit >= stop_bit)); --bit)
    {
        if (bit <= start_bit)
        {
            if ((mask & byte) == mask) value += (*inverse_pow2);
            *inverse_pow2 = (double) (*inverse_pow2/2.0);
        }
        mask = (unsigned char) (mask/2);
    }

    return (value);

}  /*  "ftx_interpret_fraction"  */

