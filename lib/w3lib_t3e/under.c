#include <sys/fpu.h>

int
get_fs_bit_( void )
{
int	m;

	m = get_fpc_csr();

	return ( (m >>24)&1 );
}

int
set_fs_bit_( void )
{
int	m, n;

	m = get_fpc_csr();

	m = set_fpc_csr( m | 0x01000000 );

	return ( (m >>24)&1 );
}
