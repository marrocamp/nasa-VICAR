#ifdef VAX
#define PDS_MAX_EXPONENT     127
#else
#define PDS_MAX_EXPONENT    1023
#endif

#define PDS_NUMBER_SIZE       36

#define PDS_TINY_INT_BYTES     1
#define PDS_SHORT_INT_BYTES    2
#define PDS_INT_BYTES          4

/* 08.30.2002 MDC - Added this macro to be able to recognize an 8 byte integer */
#define PDS_LONG_INT_BYTES	   8	

/* 10-13-02 MDC - Added this macro to be able to recognize a 6-byte integer. */
#define PDS_SIX_INT_BYTES	   6

#define PDS_IEEE_REAL_BYTES    4
#define PDS_IEEE_DOUBLE_BYTES  8
#define PDS_IEEE_TEMP_BYTES   10

#define PDS_VAXF_REAL_BYTES    4
#define PDS_VAXD_REAL_BYTES    8
#define PDS_VAXH_REAL_BYTES   16

typedef struct bit_struct
{
#ifndef SUN_UNIX
    char b7 : 1;
    char b6 : 1;
    char b5 : 1;
    char b4 : 1;
    char b3 : 1;
    char b2 : 1;
    char b1 : 1;
    char b0 : 1;
#else
    char b0 : 1;
    char b1 : 1;
    char b2 : 1;
    char b3 : 1;
    char b4 : 1;
    char b5 : 1;
    char b6 : 1;
    char b7 : 1;
#endif

}  BITS;   

typedef union byte_conversion
{
    BITS bit;
    char byte;

} BYTE_CONVERSION;


#ifndef SUN_UNIX

unsigned char *ft_lsb_binary (unsigned char *text, 
                              long size);

unsigned char *ft_msb_binary (unsigned char *text, 
                              long size);

unsigned char *ft_char (unsigned char *text, 
                        long size);

unsigned char *ft_hex (unsigned char *text, 
                       long size);

unsigned char *ft_lsb_ieee_complex (unsigned char *text, 
                                    long size);

unsigned char *ft_lsb_ieee_real (unsigned char *text, 
                                 long size);

unsigned char *ft_lsb_integer (unsigned char *text, 
                               long size, 
                               int signed_value);

unsigned char *ft_msb_ieee_complex (unsigned char *text, 
                                    long size);

unsigned char *ft_msb_ieee_real (unsigned char *text, 
                                 long size);

unsigned char *ft_msb_integer (unsigned char *text, 
                               long size, 
                               int signed_value);

unsigned char *ft_octal (unsigned char *text, 
                         long size);

unsigned char *ft_vax_complex (unsigned char *text, 
                               long size, 
                               int vaxg_type);

unsigned char *ft_vax_real (unsigned char *text, 
                            long size, 
                            int vaxg_type);


#else

unsigned char *ft_lsb_binary ();
unsigned char *ft_msb_binary ();
unsigned char *ft_char ();
unsigned char *ft_hex ();
unsigned char *ft_lsb_ieee_complex ();
unsigned char *ft_lsb_ieee_real ();
unsigned char *ft_lsb_integer ();
unsigned char *ft_msb_ieee_complex ();
unsigned char *ft_msb_ieee_real ();
unsigned char *ft_msb_integer ();
unsigned char *ft_octal ();
unsigned char *ft_vax_complex ();
unsigned char *ft_vax_real ();

#endif

