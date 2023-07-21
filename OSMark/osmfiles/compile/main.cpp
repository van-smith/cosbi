/***************************************************************************
                          main.cpp  -  description
                             -------------------
    begin                : Fri Jul  28 17:07:42 CST 2006
    copyright            : (C) 2006 by Van Smith
    email                : van@vanshardware.com
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "iostream"
#include "iomanip"  // needed for setiosflags, etc.
#include "string"
#include "stdio.h"
#include "time.h"
#include "float.h"
#include "math.h"

#define prefetch_loc(addr) \
__asm__ __volatile__ ("prefetchnta %0" \
                      : \
                      : \
                      "m" (*(((char*)(((unsigned int)(addr))&~0x7f)))))

double intArithmetic(int);
double doubleArithmetic(double, double);
//double longArithmetic(long long, long long);
double trig(double);
double io(int);
double FibTest(int n);
double PiTest(int n);
double RandomAssignment( int NumDigits );
double BandwidthIntegerRead( int NumDigits );
double BandwidthIntegerReadReverse( int NumDigits );
double BandwidthReadBP64( int NumDigits );
double BandwidthReadPrefetchNTA( int NumDigits );
double BandwidthIntegerWrite( int NumDigits );
double BandwidthIntegerWriteUnroll( int NumDigits );
double BandwidthIntegerWriteUnrollBP64( int NumDigits );
double BandwidthIntegerWriteBP64( int NumDigits );
double BandwidthIntegerCopy( int NumDigits );
double BandwidthIntegerAdd( int NumDigits );
double BandwidthIntegerScale( int NumDigits );
double BandwidthIntegerTriad( int NumDigits );
double BandwidthIntegerCopyBP64( int NumDigits );
double BandwidthDoubleRead( int NumDigits );
double BandwidthDoubleReadReverse( int NumDigits );
double BandwidthDoubleWrite( int NumDigits );
double BandwidthDoubleCopy( int NumDigits );

// arrays used for random assignment test and bandwidth tests:
long Source[ 4000000 ];
long SourceB[ 4000000 ];
long Target[ 4000000 ];
double doubleSource[ 4000000 ];
double doubleTarget[ 4000000 ];
int fDummyTarget;
double fdoubleDummyTarget;

using namespace std;

int main()
{
	int intMax =		1000000000; // 1B
	double doubleMin =  10000000000.0; // 10B
	double doubleMax =	11000000000.0; // 11B
//	long long longMin = 10000000000; // 10B
//	long long longMax = 11000000000; // 11B
	double trigMax =	10000000; // 10M
	int ioMax =			1000000; // 1M

  cout << "Hello!  Starting minibench benchmark..." << endl;

  cout  << endl
        << "Executing Pi benchmark..."
        << endl;
  double PiTestTime = PiTest( 10000 );

  cout  << endl
        << "Executing Fibonacci benchmark..."
        << endl;
  double FibTestTime = FibTest( 40 );


  cout  << endl
        << "Executing integer benchmark..."
        << endl;
  double IntegerArithmeticTime = intArithmetic(intMax);

  cout  << endl
        << "Executing double-precision floating point benchmark..."
        << endl;
  double doubleArithmeticTime = doubleArithmetic(doubleMin, doubleMax);

//	long longArithmeticTime = (long)longArithmetic(longMin, longMax);

  cout  << endl
        << "Executing transcendental functions benchmark..."
        << endl;
  double trigTime = trig(trigMax);

  cout  << endl
        << "Executing I/O benchmark..."
        << endl;
  double ioTime = io(ioMax);

  cout  << endl
        << "Executing Random Assignment benchmark..."
        << endl;
  double RandomAssignmentTime = RandomAssignment( 2000000 );

  cout  << endl
        << "Executing Memory Bandwidth Integer Read benchmark..."
        << endl;
  double BandwidthIntegerReadTime = BandwidthIntegerRead( 0 );

  cout  << endl
        << "Executing Memory Bandwidth Integer Read Reverse benchmark..."
        << endl;
  double BandwidthIntegerReadReverseTime = BandwidthIntegerReadReverse( 0 );
  
  cout  << endl
        << "Executing Memory Bandwidth Integer Read BP64 benchmark..."
        << endl;
  double BandwidthIntegerReadBP64Time = BandwidthReadBP64( 0 );
  
  cout  << endl
        << "Executing Memory Bandwidth Integer Read Prefetchnta benchmark..."
        << endl;
  double BandwidthIntegerReadPrefetchNTA = BandwidthReadPrefetchNTA( 0 );

  cout  << endl
        << "Executing Memory Bandwidth Integer Write benchmark..."
        << endl;
  double BandwidthIntegerWriteTime = BandwidthIntegerWrite( 0 );

  cout  << endl
        << "Executing Memory Bandwidth Integer Write Unroll benchmark..."
        << endl;
  double BandwidthIntegerWriteUnrollTime = BandwidthIntegerWriteUnroll( 0 );
/*
  cout  << endl
        << "Executing Memory Bandwidth Integer Write Unroll Block Prefetch 64 benchmark..."
        << endl;
  double BandwidthIntegerWriteUnrollBP64Time = BandwidthIntegerWriteUnrollBP64( 0 );
  
  cout  << endl
        << "Executing Memory Bandwidth Integer Write Block Prefetch 64 benchmark..."
        << endl;
  double BandwidthIntegerWriteBP64Time = BandwidthIntegerWriteBP64( 0 );
*/
  cout  << endl
        << "Executing Memory Bandwidth Integer Copy benchmark..."
        << endl;
  double BandwidthIntegerCopyTime = BandwidthIntegerCopy( 0 );

  cout  << endl
        << "Executing Memory Bandwidth Integer Add benchmark..."
        << endl;
  double BandwidthIntegerAddTime = BandwidthIntegerAdd( 0 );

  cout  << endl
        << "Executing Memory Bandwidth Integer Scale benchmark..."
        << endl;
  double BandwidthIntegerScaleTime = BandwidthIntegerScale( 0 );

  cout  << endl
        << "Executing Memory Bandwidth Integer Triad benchmark..."
        << endl;
  double BandwidthIntegerTriadTime = BandwidthIntegerTriad( 0 );

  cout  << endl
        << "Executing Memory Bandwidth Integer Copy BP64 benchmark..."
        << endl;
  double BandwidthIntegerCopyBP64Time = BandwidthIntegerCopyBP64( 0 );

  cout  << endl
        << "Executing Memory Bandwidth Double Read benchmark..."
        << endl;
  double BandwidthDoubleReadTime = BandwidthDoubleRead( 0 );

  cout  << endl
        << "Executing Memory Bandwidth Double Read Reverse benchmark..."
        << endl;
  double BandwidthDoubleReadReverseTime = BandwidthDoubleReadReverse( 0 );
  
  cout  << endl
        << "Executing Memory Bandwidth Double Write benchmark..."
        << endl;
  double BandwidthDoubleWriteTime = BandwidthDoubleWrite( 0 );

  cout  << endl
        << "Executing Memory Bandwidth Double Copy benchmark..."
        << endl;
  double BandwidthDoubleCopyTime = BandwidthDoubleCopy( 0 );

  // report total time:
	double totalTime = FibTestTime + IntegerArithmeticTime + doubleArithmeticTime
    + trigTime + ioTime + PiTestTime + RandomAssignmentTime 
    + BandwidthIntegerReadTime + BandwidthIntegerWriteTime
    + BandwidthIntegerWriteUnrollTime //+ BandwidthIntegerWriteUnrollBP64Time + BandwidthIntegerWriteBP64Time 
    + BandwidthIntegerTriadTime
    + BandwidthIntegerScaleTime + BandwidthIntegerAddTime 
    + BandwidthIntegerCopyTime + BandwidthIntegerCopyBP64Time + 
    + BandwidthIntegerReadPrefetchNTA + BandwidthIntegerReadReverseTime
    + BandwidthDoubleReadTime + BandwidthDoubleWriteTime
    + BandwidthDoubleCopyTime + BandwidthDoubleReadReverseTime;

  // set number formatting:
  cout << setiosflags( ios::fixed )
       << setprecision( 2 );

  cout << endl;
  cout << "*** SUMMARY (test, test time (ms)) ***" << endl;
  cout << "=====================================" << endl;
  cout << "Pi test \t" << PiTestTime << endl;
  cout << "Fib test \t" << FibTestTime << endl;
  cout << "Integer arithmetic test \t" << IntegerArithmeticTime << endl;
  cout << "Double arithmetic test \t" << doubleArithmeticTime << endl;
  cout << "Transcendental function test \t" << trigTime << endl;
  cout << "I/O test \t" << ioTime << endl;
  cout << "Random Assignment test \t" << RandomAssignmentTime << endl;
  cout << "Memory Bandwidth Integer Read test \t" << BandwidthIntegerReadTime << endl;
  cout << "Memory Bandwidth Integer Read BP64 test \t" << BandwidthIntegerReadBP64Time << endl;
  cout << "Memory Bandwidth Integer Read PREFETCHNTA test \t" << BandwidthIntegerReadPrefetchNTA << endl;
  cout << "Memory Bandwidth Integer Read Reverse test \t" << BandwidthIntegerReadReverseTime << endl;
  cout << "Memory Bandwidth Integer Write test \t" << BandwidthIntegerWriteTime << endl;
  cout << "Memory Bandwidth Integer Write Unroll test \t" << BandwidthIntegerWriteUnrollTime << endl;
//  cout << "Memory Bandwidth Integer Write Unroll BP64 test \t" << BandwidthIntegerWriteUnrollBP64Time << endl;
  cout << "Memory Bandwidth Integer Copy test \t" << BandwidthIntegerCopyTime << endl;
  cout << "Memory Bandwidth Integer Add test \t" << BandwidthIntegerAddTime << endl;
  cout << "Memory Bandwidth Integer Scale test \t" << BandwidthIntegerScaleTime << endl;
  cout << "Memory Bandwidth Integer Triad test \t" << BandwidthIntegerTriadTime << endl;
  cout << "Memory Bandwidth Integer Copy BP64 test \t" << BandwidthIntegerCopyBP64Time << endl;
  cout << "Memory Bandwidth Double Read test \t" << BandwidthDoubleReadTime << endl;
  cout << "Memory Bandwidth Double Read Reverse test \t" << BandwidthDoubleReadReverseTime << endl;
  cout << "Memory Bandwidth Double Write test \t" << BandwidthDoubleWriteTime << endl;
  cout << "Memory Bandwidth Double Copy test \t" << BandwidthDoubleCopyTime << endl;

  cout  << "************************************"
        << endl
        << "Total elapsed time: "
        << totalTime
        << " ms." << endl;

  cout  << "CLOCKS_PER_SEC: " << CLOCKS_PER_SEC << endl;

	cout << endl << "Minibench benchmark completed." << endl;

  remove( "DeleteMe_minibench.txt" );
  cout << "Press <Enter> to terminate program." << endl;
  cin.get();
  return 0;
}

string ComputePi(int NumDigits)
{
  int I;
  int J;
  int K;
  int P;
  int Q;
  int X;
  int Nines;
  int Predigit;
  int arraySize;
  arraySize = ( 10 * NumDigits ) / 3;
  int * A = new int [ arraySize ];
  int PiLength;
  string strResult;
  // initialize string to "-----..."
  for ( I = 0; I <= NumDigits; I++ )
  {
    strResult += "-";
  }

  PiLength = 1;
  Nines = 0;
  Predigit = 0;
  // intitialize array:
  for ( I = 0; I <= arraySize; I++ )
    A[I] = 2; // ok

  for ( J = 0; J <= (NumDigits-1); J++ )
  {
    Q = 0;
    P = 2 * arraySize - 1;
    for ( I = (arraySize - 1); I >= 0; I-- )
    {
      X = ( 10*A[I] ) + ( Q*(I+1) );
      A[I] = X % P;
      Q = X / P;
      P = P - 2;
    }
    A[0] = Q % 10;
    Q = Q / 10;
    if ( Q == 9 )
    {
      Nines++;
    }
    else
    {
      if ( Q == 10 )
      {
        strResult[PiLength - 1] = char(Predigit + 1 + int('0'));
        for ( K = 1; K <= Nines; K++ )
          strResult[ PiLength - 1 + K ] = '0';
        PiLength = PiLength + Nines + 1;
        Predigit = 0;
        Nines = 0;
      }
      else
      {
        strResult[PiLength - 1] = char(Predigit + int('0'));
        Predigit = Q;
        for ( K = 1; K <= Nines; K++ )
          strResult[ PiLength - 1 + K ] = '9';
        PiLength = PiLength + Nines + 1;
        Nines = 0;
      }
    }
  }
  strResult[PiLength - 1] = char( Predigit + int('0') );

  return strResult;

  delete [] A;
}

double PiTest(int n)
{
	double elapsedTime;
	clock_t stopTime;
	clock_t startTime;
	string strResult;

	startTime = clock();
	strResult = ComputePi( n );
	stopTime = clock();

	elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "strResult: "
        << strResult
        << endl;
  cout  << "Calculate Pi elapsed time: "
        << elapsedTime
        << " ms with n of "
        << n
        << endl;

  return elapsedTime;
}


int Fibonacci(int n)
{
  if (n > 2)
  {
    return Fibonacci( n - 1 ) + Fibonacci( n - 2 );
  }
  else
  {
    return 1;
  }
}

double FibTest(int n)
{
	double elapsedTime;
	clock_t stopTime;
	clock_t startTime;
  int intResult;

	startTime = clock();
	intResult = Fibonacci( n );
	stopTime = clock();

	elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);

  cout << setiosflags( ios::fixed )
       << setprecision( 0 );
  cout  << "Fibonacci elapsed time: "
        << elapsedTime
        << " ms with n of "
        << n
        << endl;
  cout  << "intResult: "
        << intResult
        << endl;

  return elapsedTime;
}

double intArithmetic(int intMax)
{
	double elapsedTime;
	clock_t stopTime;
	clock_t startTime = clock();

	int intResult = 1;
	int i = 1;
	while (i < intMax)
	{
		intResult -= i++;
		intResult += i++;
		intResult *= i++;
		intResult /= i++;
	}

	stopTime = clock();
	elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Integer arithmetic elapsed time: "
        << elapsedTime
        << " ms with int Max of "
        << intMax
        << endl;
  cout  << "i: "
        << i
        << endl;
  cout  << "intResult: "
        << intResult
        << endl;

  return elapsedTime;
}


double doubleArithmetic(double doubleMin, double doubleMax)
{
	double elapsedTime;
	clock_t stopTime;
	clock_t startTime = clock();

	double doubleResult = doubleMin;
	double i = doubleMin;
	while (i < doubleMax)
	{
		doubleResult -= i++;
		doubleResult += i++;
		doubleResult *= i++;
		doubleResult /= i++;
	}

	stopTime = clock();
	elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);

  cout  << "Double-precision floating point arithmetic elapsed time: "
        << elapsedTime
        << " ms with doubleMin "
        << setiosflags( ios::fixed )
        << setprecision( 6 )
        << doubleMin
        << ", doubleMax "
        << doubleMax
        << endl;
  cout  << "i: "
        << i
        << endl;
  cout  << "doubleResult: "
        << doubleResult
        << endl;

	return elapsedTime;
}


/*
double longArithmetic(long long longMin, long long longMax)
{
	double elapsedTime;
	clock_t stopTime;
	clock_t startTime = clock();

	long long longResult = longMin;
	long long i = longMin;
	while (i < longMax)
	{
		longResult -= i++;
		longResult += i++;
		longResult *= i++;
		longResult /= i++;
	}

	stopTime = clock();
	elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
	printf("Long arithmetic elapsed time: %1.0f ms with longMax %I64d\n", elapsedTime, longMax);
	printf(" i: %I64d\n", i);
	printf(" longResult: %I64d\n", longResult);
	return elapsedTime;
}
*/

double trig(double trigMax)
{
	double elapsedTime;
	clock_t stopTime;
	clock_t startTime = clock();

	double sine       = 0;
	double cosine     = 0;
	double tangent    = 0;
	double logarithm  = 0;
	double squareRoot = 0;

	double i = 0.0;
	while (i < trigMax)
	{
		sine = sin(i);
		cosine = cos(i);
		tangent = tan(i);
		logarithm = log10(i);
		squareRoot = sqrt(i);
		i++;
	}
	stopTime = clock();
	elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);

  cout << setiosflags( ios::fixed )
       << setprecision( 0 );
  cout  << "Transcendental function floating point arithmetic elapsed time: "
        << elapsedTime
        << " ms with max of "
        << trigMax
        << endl;
  cout << setiosflags( ios::fixed )
       << setprecision( 6 );
  cout  << "i: "
        << i
        << endl;
  cout  << "sine: "
        << sine
        << endl;
  cout  << "cosine: "
        << cosine
        << endl;
  cout  << "tangent: "
        << tangent
        << endl;
  cout  << "logarithm: "
        << logarithm
        << endl;
  cout  << "squareRoot: "
        << squareRoot
        << endl;

	return elapsedTime;
}

double io(int ioMax)
{
	double elapsedTime;
	clock_t stopTime;

  clock_t startTime = clock();
	FILE *stream;
	stream = fopen("DeleteMe_minibench.txt", "w");
	int i = 0;
	while (i++ < ioMax)
	{
		fputs("abcdefghijklmnopqrstuvwxyz1234567890abcdefghijklmnopqrstuvwxyz1234567890abcdefgh\n", stream);
	}
	fclose(stream);

	char readLine[100];
	stream = fopen("DeleteMe_minibench.txt", "r");
	i = 0;
	while (i++ < ioMax)
	{
		fgets(readLine, 100, stream);
	}
	fclose(stream);
	stopTime = clock();

 	elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "I/O elapsed time: "
        << elapsedTime
        << " ms with max of "
        << ioMax
        << endl;
  cout  << "i:"
        << i
        << endl;
  cout  << "readLine: "
        << readLine
        << endl;

	return elapsedTime;
}

double RandomAssignment( int NumDigits )
{
  int liIterate;
  int liFill;
  int liRandomSource;
  int liRandomTarget;
  double elapsedTime;
  const int TWO_MILLION = 2097152;
  const int ITERATIONS = 10;
  const int BYTES_PER_ARRAY = 4 * TWO_MILLION;

  for ( liFill = 0; liFill < 20; liFill++ )
  {
    Source[ liFill ] = rand();
  }
  
  clock_t stopTime;
  clock_t startTime = clock();
  for ( liIterate = 0; liIterate <= ITERATIONS; liIterate++ )
  {
    for ( liFill = 0; liFill <= TWO_MILLION; liFill++ )
    {
      liRandomSource = (int)(TWO_MILLION * ((double) rand() / (double) RAND_MAX));
      liRandomTarget = (int)(TWO_MILLION * ((double) rand() / (double) RAND_MAX));
      Target[ liRandomTarget ] = Source[ liRandomSource ];
    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  double memoryBandwidth = 
	  ( 2 * 4 * ITERATIONS * (double)TWO_MILLION ) / ( elapsedTime * (double) 1000.0 );

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Random Assignment elapsed time: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Block size " << BYTES_PER_ARRAY << " bytes source, " << BYTES_PER_ARRAY << " bytes target."
        << endl;
  cout  << "Memory bandwidth for random assignment: "
		<< memoryBandwidth
		<< " MB/s"
		<< endl;

  return elapsedTime;
}

double BandwidthIntegerWrite( int NumDigits )
{
  int liIterate;
  int liFill;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2000000;
  double elapsedTime;
  double memoryBandwidth;

  clock_t stopTime;
  clock_t startTime = clock();
  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
    {
      Target[ liFill ] = liIterate;
    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = ( 4 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Integer Write: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth Integer Write: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}

double BandwidthIntegerWriteUnroll( int NumDigits )
{
  int liIterate;
  int liFill;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2097152;
  double elapsedTime;
  double memoryBandwidth;

  clock_t stopTime;
  clock_t startTime = clock();
  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    for ( liFill = 0; liFill <= NUMBER_OF_ARRAY_ELEMENTS; liFill += 128 )
    {
      Target[ liFill ] = liIterate;
      Target[ liFill + 1 ] = liIterate;
      Target[ liFill + 2 ] = liIterate;
      Target[ liFill + 3 ] = liIterate;
      Target[ liFill + 4 ] = liIterate;
      Target[ liFill + 5 ] = liIterate;
      Target[ liFill + 6 ] = liIterate;
      Target[ liFill + 7 ] = liIterate;
      Target[ liFill + 8 ] = liIterate;
      Target[ liFill + 9 ] = liIterate;
      Target[ liFill + 10 ] = liIterate;
      Target[ liFill + 11 ] = liIterate;
      Target[ liFill + 12 ] = liIterate;
      Target[ liFill + 13 ] = liIterate;
      Target[ liFill + 14 ] = liIterate;
      Target[ liFill + 15 ] = liIterate;

      Target[ liFill + 16 ] = liIterate;
      Target[ liFill + 17 ] = liIterate;
      Target[ liFill + 18 ] = liIterate;
      Target[ liFill + 19 ] = liIterate;
      Target[ liFill + 20 ] = liIterate;
      Target[ liFill + 21 ] = liIterate;
      Target[ liFill + 22 ] = liIterate;
      Target[ liFill + 23 ] = liIterate;
      Target[ liFill + 24 ] = liIterate;
      Target[ liFill + 25 ] = liIterate;
      Target[ liFill + 26 ] = liIterate;
      Target[ liFill + 27 ] = liIterate;
      Target[ liFill + 28 ] = liIterate;
      Target[ liFill + 29 ] = liIterate;
      Target[ liFill + 30 ] = liIterate;
      Target[ liFill + 31 ] = liIterate;

      Target[ liFill + 32 ] = liIterate;
      Target[ liFill + 33 ] = liIterate;
      Target[ liFill + 34 ] = liIterate;
      Target[ liFill + 35 ] = liIterate;
      Target[ liFill + 36 ] = liIterate;
      Target[ liFill + 37 ] = liIterate;
      Target[ liFill + 38 ] = liIterate;
      Target[ liFill + 39 ] = liIterate;
      Target[ liFill + 40 ] = liIterate;
      Target[ liFill + 41 ] = liIterate;
      Target[ liFill + 42 ] = liIterate;
      Target[ liFill + 43 ] = liIterate;
      Target[ liFill + 44 ] = liIterate;
      Target[ liFill + 45 ] = liIterate;
      Target[ liFill + 46 ] = liIterate;
      Target[ liFill + 47 ] = liIterate;

      Target[ liFill + 48 ] = liIterate;
      Target[ liFill + 49 ] = liIterate;
      Target[ liFill + 50 ] = liIterate;
      Target[ liFill + 51 ] = liIterate;
      Target[ liFill + 52 ] = liIterate;
      Target[ liFill + 53 ] = liIterate;
      Target[ liFill + 54 ] = liIterate;
      Target[ liFill + 55 ] = liIterate;
      Target[ liFill + 56 ] = liIterate;
      Target[ liFill + 57 ] = liIterate;
      Target[ liFill + 58 ] = liIterate;
      Target[ liFill + 59 ] = liIterate;
      Target[ liFill + 60 ] = liIterate;
      Target[ liFill + 61 ] = liIterate;
      Target[ liFill + 62 ] = liIterate;
      Target[ liFill + 63 ] = liIterate;

      Target[ liFill + 64 ] = liIterate;
      Target[ liFill + 65 ] = liIterate;
      Target[ liFill + 66 ] = liIterate;
      Target[ liFill + 67 ] = liIterate;
      Target[ liFill + 68 ] = liIterate;
      Target[ liFill + 69 ] = liIterate;
      Target[ liFill + 70 ] = liIterate;
      Target[ liFill + 71 ] = liIterate;
      Target[ liFill + 72 ] = liIterate;
      Target[ liFill + 73 ] = liIterate;
      Target[ liFill + 74 ] = liIterate;
      Target[ liFill + 75 ] = liIterate;
      Target[ liFill + 76 ] = liIterate;
      Target[ liFill + 77 ] = liIterate;
      Target[ liFill + 78 ] = liIterate;
      Target[ liFill + 79 ] = liIterate;

      Target[ liFill + 80 ] = liIterate;
      Target[ liFill + 81 ] = liIterate;
      Target[ liFill + 82 ] = liIterate;
      Target[ liFill + 83 ] = liIterate;
      Target[ liFill + 84 ] = liIterate;
      Target[ liFill + 85 ] = liIterate;
      Target[ liFill + 86 ] = liIterate;
      Target[ liFill + 87 ] = liIterate;
      Target[ liFill + 88 ] = liIterate;
      Target[ liFill + 89 ] = liIterate;
      Target[ liFill + 90 ] = liIterate;
      Target[ liFill + 91 ] = liIterate;
      Target[ liFill + 92 ] = liIterate;
      Target[ liFill + 93 ] = liIterate;
      Target[ liFill + 94 ] = liIterate;
      Target[ liFill + 95 ] = liIterate;

      Target[ liFill + 96 ] = liIterate;
      Target[ liFill + 97 ] = liIterate;
      Target[ liFill + 98 ] = liIterate;
      Target[ liFill + 99 ] = liIterate;
      Target[ liFill + 100 ] = liIterate;
      Target[ liFill + 101 ] = liIterate;
      Target[ liFill + 102 ] = liIterate;
      Target[ liFill + 103 ] = liIterate;
      Target[ liFill + 104 ] = liIterate;
      Target[ liFill + 105 ] = liIterate;
      Target[ liFill + 106 ] = liIterate;
      Target[ liFill + 107 ] = liIterate;
      Target[ liFill + 108 ] = liIterate;
      Target[ liFill + 109 ] = liIterate;
      Target[ liFill + 110 ] = liIterate;
      Target[ liFill + 111 ] = liIterate;

      Target[ liFill + 112 ] = liIterate;
      Target[ liFill + 113 ] = liIterate;
      Target[ liFill + 114 ] = liIterate;
      Target[ liFill + 115 ] = liIterate;
      Target[ liFill + 116 ] = liIterate;
      Target[ liFill + 117 ] = liIterate;
      Target[ liFill + 118 ] = liIterate;
      Target[ liFill + 119 ] = liIterate;
      Target[ liFill + 120 ] = liIterate;
      Target[ liFill + 121 ] = liIterate;
      Target[ liFill + 122 ] = liIterate;
      Target[ liFill + 123 ] = liIterate;
      Target[ liFill + 124 ] = liIterate;
      Target[ liFill + 125 ] = liIterate;
      Target[ liFill + 126 ] = liIterate;
      Target[ liFill + 127 ] = liIterate;

    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = ( 4 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Integer Write, Loop Unroll: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth Integer Write Loop Unroll: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}

double BandwidthIntegerWriteUnrollBP64( int NumDigits )
{
  int liIterate;
  int liFill;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2097152;
  double elapsedTime;
  double memoryBandwidth;

  clock_t stopTime;
  clock_t startTime = clock();
  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    for ( liFill = 0; liFill <= NUMBER_OF_ARRAY_ELEMENTS; liFill += 128 )
    {
      // prefetch 8 cache lines:
      Target[ liFill ] = liIterate;
      Target[ liFill + 16 ] = liIterate;
      Target[ liFill + 32 ] = liIterate;
      Target[ liFill + 48 ] = liIterate;
      Target[ liFill + 64 ] = liIterate;
      Target[ liFill + 80 ] = liIterate;
      Target[ liFill + 96 ] = liIterate;
      Target[ liFill + 112 ] = liIterate;
    
      // now walk through the array:
//      Target[ liFill ] = liIterate;
      Target[ liFill + 1 ] = liIterate;
      Target[ liFill + 2 ] = liIterate;
      Target[ liFill + 3 ] = liIterate;
      Target[ liFill + 4 ] = liIterate;
      Target[ liFill + 5 ] = liIterate;
      Target[ liFill + 6 ] = liIterate;
      Target[ liFill + 7 ] = liIterate;
      Target[ liFill + 8 ] = liIterate;
      Target[ liFill + 9 ] = liIterate;
      Target[ liFill + 10 ] = liIterate;
      Target[ liFill + 11 ] = liIterate;
      Target[ liFill + 12 ] = liIterate;
      Target[ liFill + 13 ] = liIterate;
      Target[ liFill + 14 ] = liIterate;
      Target[ liFill + 15 ] = liIterate;

//      Target[ liFill + 16 ] = liIterate;
      Target[ liFill + 17 ] = liIterate;
      Target[ liFill + 18 ] = liIterate;
      Target[ liFill + 19 ] = liIterate;
      Target[ liFill + 20 ] = liIterate;
      Target[ liFill + 21 ] = liIterate;
      Target[ liFill + 22 ] = liIterate;
      Target[ liFill + 23 ] = liIterate;
      Target[ liFill + 24 ] = liIterate;
      Target[ liFill + 25 ] = liIterate;
      Target[ liFill + 26 ] = liIterate;
      Target[ liFill + 27 ] = liIterate;
      Target[ liFill + 28 ] = liIterate;
      Target[ liFill + 29 ] = liIterate;
      Target[ liFill + 30 ] = liIterate;
      Target[ liFill + 31 ] = liIterate;

//      Target[ liFill + 32 ] = liIterate;
      Target[ liFill + 33 ] = liIterate;
      Target[ liFill + 34 ] = liIterate;
      Target[ liFill + 35 ] = liIterate;
      Target[ liFill + 36 ] = liIterate;
      Target[ liFill + 37 ] = liIterate;
      Target[ liFill + 38 ] = liIterate;
      Target[ liFill + 39 ] = liIterate;
      Target[ liFill + 40 ] = liIterate;
      Target[ liFill + 41 ] = liIterate;
      Target[ liFill + 42 ] = liIterate;
      Target[ liFill + 43 ] = liIterate;
      Target[ liFill + 44 ] = liIterate;
      Target[ liFill + 45 ] = liIterate;
      Target[ liFill + 46 ] = liIterate;
      Target[ liFill + 47 ] = liIterate;

//      Target[ liFill + 48 ] = liIterate;
      Target[ liFill + 49 ] = liIterate;
      Target[ liFill + 50 ] = liIterate;
      Target[ liFill + 51 ] = liIterate;
      Target[ liFill + 52 ] = liIterate;
      Target[ liFill + 53 ] = liIterate;
      Target[ liFill + 54 ] = liIterate;
      Target[ liFill + 55 ] = liIterate;
      Target[ liFill + 56 ] = liIterate;
      Target[ liFill + 57 ] = liIterate;
      Target[ liFill + 58 ] = liIterate;
      Target[ liFill + 59 ] = liIterate;
      Target[ liFill + 60 ] = liIterate;
      Target[ liFill + 61 ] = liIterate;
      Target[ liFill + 62 ] = liIterate;
      Target[ liFill + 63 ] = liIterate;

//      Target[ liFill + 64 ] = liIterate;
      Target[ liFill + 65 ] = liIterate;
      Target[ liFill + 66 ] = liIterate;
      Target[ liFill + 67 ] = liIterate;
      Target[ liFill + 68 ] = liIterate;
      Target[ liFill + 69 ] = liIterate;
      Target[ liFill + 70 ] = liIterate;
      Target[ liFill + 71 ] = liIterate;
      Target[ liFill + 72 ] = liIterate;
      Target[ liFill + 73 ] = liIterate;
      Target[ liFill + 74 ] = liIterate;
      Target[ liFill + 75 ] = liIterate;
      Target[ liFill + 76 ] = liIterate;
      Target[ liFill + 77 ] = liIterate;
      Target[ liFill + 78 ] = liIterate;
      Target[ liFill + 79 ] = liIterate;

//      Target[ liFill + 80 ] = liIterate;
      Target[ liFill + 81 ] = liIterate;
      Target[ liFill + 82 ] = liIterate;
      Target[ liFill + 83 ] = liIterate;
      Target[ liFill + 84 ] = liIterate;
      Target[ liFill + 85 ] = liIterate;
      Target[ liFill + 86 ] = liIterate;
      Target[ liFill + 87 ] = liIterate;
      Target[ liFill + 88 ] = liIterate;
      Target[ liFill + 89 ] = liIterate;
      Target[ liFill + 90 ] = liIterate;
      Target[ liFill + 91 ] = liIterate;
      Target[ liFill + 92 ] = liIterate;
      Target[ liFill + 93 ] = liIterate;
      Target[ liFill + 94 ] = liIterate;
      Target[ liFill + 95 ] = liIterate;

//      Target[ liFill + 96 ] = liIterate;
      Target[ liFill + 97 ] = liIterate;
      Target[ liFill + 98 ] = liIterate;
      Target[ liFill + 99 ] = liIterate;
      Target[ liFill + 100 ] = liIterate;
      Target[ liFill + 101 ] = liIterate;
      Target[ liFill + 102 ] = liIterate;
      Target[ liFill + 103 ] = liIterate;
      Target[ liFill + 104 ] = liIterate;
      Target[ liFill + 105 ] = liIterate;
      Target[ liFill + 106 ] = liIterate;
      Target[ liFill + 107 ] = liIterate;
      Target[ liFill + 108 ] = liIterate;
      Target[ liFill + 109 ] = liIterate;
      Target[ liFill + 110 ] = liIterate;
      Target[ liFill + 111 ] = liIterate;

//      Target[ liFill + 112 ] = liIterate;
      Target[ liFill + 113 ] = liIterate;
      Target[ liFill + 114 ] = liIterate;
      Target[ liFill + 115 ] = liIterate;
      Target[ liFill + 116 ] = liIterate;
      Target[ liFill + 117 ] = liIterate;
      Target[ liFill + 118 ] = liIterate;
      Target[ liFill + 119 ] = liIterate;
      Target[ liFill + 120 ] = liIterate;
      Target[ liFill + 121 ] = liIterate;
      Target[ liFill + 122 ] = liIterate;
      Target[ liFill + 123 ] = liIterate;
      Target[ liFill + 124 ] = liIterate;
      Target[ liFill + 125 ] = liIterate;
      Target[ liFill + 126 ] = liIterate;
      Target[ liFill + 127 ] = liIterate;

    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = ( 4 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Integer Write, Loop Unroll, Block Prefetch 64: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth Integer Write Loop Unroll Block Prefetch 64: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}

double BandwidthIntegerWriteBP64( int NumDigits )
{
  int liIterate;
  int i;
  int j;
  int liPrefetch;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2097152;
  const int STEP_SIZE = 8192;
  double elapsedTime;
  double memoryBandwidth;

  clock_t stopTime;
  clock_t startTime = clock();
  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    // we need to step through the array in chunks of 512 64-byte cache lines.
    // this means 512 * 64 / 4 = 8192 integer elements at a time.
    // so we need an outer loop that steps through the array at 8,192 element
    // chunks:
    for ( i = 0; i < NUMBER_OF_ARRAY_ELEMENTS - STEP_SIZE; i += STEP_SIZE )
    {
      // prefetch 512 64-byte cache lines:
      for ( liPrefetch = i; liPrefetch < i + STEP_SIZE; liPrefetch += 256 )
      {
        Target[ liPrefetch ] = liIterate;               
        Target[ liPrefetch + 16 ] = liIterate;               
        Target[ liPrefetch + 32 ] = liIterate;               
        Target[ liPrefetch + 48 ] = liIterate;               
        Target[ liPrefetch + 64 ] = liIterate;               
        Target[ liPrefetch + 80 ] = liIterate;               
        Target[ liPrefetch + 96 ] = liIterate;               
        Target[ liPrefetch + 112 ] = liIterate;               
        Target[ liPrefetch + 128 ] = liIterate;               
        Target[ liPrefetch + 144 ] = liIterate;               
        Target[ liPrefetch + 160 ] = liIterate;               
        Target[ liPrefetch + 176 ] = liIterate;               
        Target[ liPrefetch + 192 ] = liIterate;               
        Target[ liPrefetch + 208 ] = liIterate;               
        Target[ liPrefetch + 224 ] = liIterate;               
        Target[ liPrefetch + 240 ] = liIterate;               
      }      
      // now we need to go through the next 8,192 byte chunk:
      for ( j = i; j < i + STEP_SIZE; j++ )
      {
        Target[ j ] = liIterate;               
      }      
    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = ( 4 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Integer Write, Block Prefetch 64: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth Integer Write Block Prefetch 64: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}

double BandwidthIntegerCopyBP64( int NumDigits )
{
  int liIterate;
  int i;
  int j;
  int liPrefetch;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2097152;
  const int STEP_SIZE = 8192;
  double elapsedTime;
  double memoryBandwidth;

  // fill the source array with crap:
  for ( i = 0; i < NUMBER_OF_ARRAY_ELEMENTS; i++ )
  {
    Source[ i ] = rand();
  }

  clock_t stopTime;
  clock_t startTime = clock();

  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    // we need to step through the array in chunks of 512 64-byte cache lines.
    // this means 512 * 64 / 4 = 8192 integer elements at a time.
    // so we need an outer loop that steps through the array at 8,192 element
    // chunks:
    for ( i = 0; i < NUMBER_OF_ARRAY_ELEMENTS - STEP_SIZE; i += STEP_SIZE )
    {
      // prefetch 512 64-byte cache lines:
      for ( liPrefetch = i; liPrefetch < i + STEP_SIZE; liPrefetch += 256 )
      {
        Target[ liPrefetch ] = Source[ liPrefetch ];               
        Target[ liPrefetch + 16 ] = Source[ liPrefetch + 16 ];               
        Target[ liPrefetch + 32 ] = Source[ liPrefetch + 32 ];               
        Target[ liPrefetch + 48 ] = Source[ liPrefetch + 48 ];               
        Target[ liPrefetch + 64 ] = Source[ liPrefetch + 64 ];               
        Target[ liPrefetch + 80 ] = Source[ liPrefetch + 80 ];               
        Target[ liPrefetch + 96 ] = Source[ liPrefetch + 96 ];               
        Target[ liPrefetch + 112 ] = Source[ liPrefetch + 112 ];               
        Target[ liPrefetch + 128 ] = Source[ liPrefetch + 128 ];               
        Target[ liPrefetch + 144 ] = Source[ liPrefetch + 144 ];               
        Target[ liPrefetch + 160 ] = Source[ liPrefetch + 160 ];               
        Target[ liPrefetch + 176 ] = Source[ liPrefetch + 176 ];               
        Target[ liPrefetch + 192 ] = Source[ liPrefetch + 192 ];               
        Target[ liPrefetch + 208 ] = Source[ liPrefetch + 208 ];               
        Target[ liPrefetch + 224 ] = Source[ liPrefetch + 224 ];               
        Target[ liPrefetch + 240 ] = Source[ liPrefetch + 240 ];               
      }      
      // now we need to go through the next 8,192 byte chunk:
      for ( j = i; j < i + STEP_SIZE; j += 16 )
      {
        Target[ j ] = Source[ j ];               
        Target[ j + 1 ] = Source[ j + 1 ];               
        Target[ j + 2 ] = Source[ j + 2 ];               
        Target[ j + 3 ] = Source[ j + 3 ];               
        Target[ j + 4 ] = Source[ j + 4 ];               
        Target[ j + 5 ] = Source[ j + 5 ];               
        Target[ j + 6 ] = Source[ j + 6 ];               
        Target[ j + 7 ] = Source[ j + 7 ];               
        Target[ j + 8 ] = Source[ j + 8 ];               
        Target[ j + 9 ] = Source[ j + 9 ];               
        Target[ j + 10 ] = Source[ j + 10 ];               
        Target[ j + 11 ] = Source[ j + 11 ];               
        Target[ j + 12 ] = Source[ j + 12 ];               
        Target[ j + 13 ] = Source[ j + 13 ];               
        Target[ j + 14 ] = Source[ j + 14 ];               
        Target[ j + 15 ] = Source[ j + 15 ];               
      }      
    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = ( 2 * 4 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Integer Copy, Block Prefetch 64: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth Integer Copy Block Prefetch 64: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}

double BandwidthIntegerRead( int NumDigits )
{
  int liIterate;
  int liFill;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2000000;
  double elapsedTime;
  double memoryBandwidth;
  
  // fill the array with crap:
  for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
  {
    Target[ liFill ] = rand();
  }

  clock_t stopTime;
  clock_t startTime = clock();
  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
    {
      fDummyTarget = Target[ liFill ];
    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = ( 4 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Integer Read: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth Integer Read: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}

double BandwidthIntegerReadReverse( int NumDigits )
{
  int liIterate;
  int liFill;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2000000;
  double elapsedTime;
  double memoryBandwidth;
  
  // fill the array with crap:
  for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
  {
    Target[ liFill ] = rand();
  }

  clock_t stopTime;
  clock_t startTime = clock();
  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    for ( liFill = NUMBER_OF_ARRAY_ELEMENTS - 1; liFill >= 0; liFill-- )
    {
      fDummyTarget = Target[ liFill ];
    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = ( 4 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Integer Read, Reverse Stride: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth Integer Read, Reverse Stride: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}

double BandwidthReadBP64( int NumDigits )
{
  int liIterate;
  int i;
  int j;
  int liPrefetch;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2097152;
  const int STEP_SIZE = 8192;
  double elapsedTime;
  double memoryBandwidth;

  // fill the source array with crap:
  for ( i = 0; i < NUMBER_OF_ARRAY_ELEMENTS; i++ )
  {
    Source[ i ] = rand();
  }

  clock_t stopTime;
  clock_t startTime = clock();

  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    // we need to step through the array in chunks of 512 64-byte cache lines.
    // this means 512 * 64 / 4 = 8192 integer elements at a time.
    // so we need an outer loop that steps through the array at 8,192 element
    // chunks:
    for ( i = 0; i < NUMBER_OF_ARRAY_ELEMENTS - STEP_SIZE; i += STEP_SIZE )
    {
      // prefetch 512 64-byte cache lines:
      for ( liPrefetch = i; liPrefetch < i + STEP_SIZE; liPrefetch += 256 )
      {
        fDummyTarget = Source[ liPrefetch ];               
        fDummyTarget = Source[ liPrefetch + 16 ];               
        fDummyTarget = Source[ liPrefetch + 32 ];               
        fDummyTarget = Source[ liPrefetch + 48 ];               
        fDummyTarget = Source[ liPrefetch + 64 ];               
        fDummyTarget = Source[ liPrefetch + 80 ];               
        fDummyTarget = Source[ liPrefetch + 96 ];               
        fDummyTarget = Source[ liPrefetch + 112 ];               
        fDummyTarget = Source[ liPrefetch + 128 ];               
        fDummyTarget = Source[ liPrefetch + 144 ];               
        fDummyTarget = Source[ liPrefetch + 160 ];               
        fDummyTarget = Source[ liPrefetch + 176 ];               
        fDummyTarget = Source[ liPrefetch + 192 ];               
        fDummyTarget = Source[ liPrefetch + 208 ];               
        fDummyTarget = Source[ liPrefetch + 224 ];               
        fDummyTarget = Source[ liPrefetch + 240 ];               
      }      
      // now we need to go through the next 8,192 byte chunk:
      for ( j = i; j < i + STEP_SIZE; j += 16 )
      {
        fDummyTarget = Source[ j ];               
        fDummyTarget = Source[ j + 1 ];               
        fDummyTarget = Source[ j + 2 ];               
        fDummyTarget = Source[ j + 3 ];               
        fDummyTarget = Source[ j + 4 ];               
        fDummyTarget = Source[ j + 5 ];               
        fDummyTarget = Source[ j + 6 ];               
        fDummyTarget = Source[ j + 7 ];               
        fDummyTarget = Source[ j + 8 ];               
        fDummyTarget = Source[ j + 9 ];               
        fDummyTarget = Source[ j + 10 ];               
        fDummyTarget = Source[ j + 11 ];               
        fDummyTarget = Source[ j + 12 ];               
        fDummyTarget = Source[ j + 13 ];               
        fDummyTarget = Source[ j + 14 ];               
        fDummyTarget = Source[ j + 15 ];               
      }      
    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = ( 4 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Integer Read, Block Prefetch 64: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth Integer Read, Block Prefetch 64: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}

double BandwidthReadPrefetchNTA( int NumDigits )
{
  int liIterate;
  int i;
  int j;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2097152;
  const int STEP_SIZE = 256; // 8192;
  double elapsedTime;
  double memoryBandwidth;
  // fill the source array with crap:
  for ( i = 0; i < NUMBER_OF_ARRAY_ELEMENTS; i++ )
  {
    Source[ i ] = rand();
  }

  clock_t stopTime;
  clock_t startTime = clock();

  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    // we need to step through the array in chunks of 4 64-byte cache lines.
    // this means 4 * 64 / 4 = 64 integer elements at a time.
    // so we need an outer loop that steps through the array at 64 element
    // chunks:
    for ( i = 0; i < NUMBER_OF_ARRAY_ELEMENTS - STEP_SIZE; i += STEP_SIZE )
    {
      prefetch_loc( Source[ i ] );
      prefetch_loc( Source[ i + 16 ] );
      prefetch_loc( Source[ i + 32 ] );
      prefetch_loc( Source[ i + 48 ] );
      // now we need to go through the next 256 byte chunk:
      for ( j = i; j < i + STEP_SIZE; j += 16 )
      {
        fDummyTarget = Source[ j ];               
        fDummyTarget = Source[ j + 1 ];               
        fDummyTarget = Source[ j + 2 ];               
        fDummyTarget = Source[ j + 3 ];               
        fDummyTarget = Source[ j + 4 ];               
        fDummyTarget = Source[ j + 5 ];               
        fDummyTarget = Source[ j + 6 ];               
        fDummyTarget = Source[ j + 7 ];               
        fDummyTarget = Source[ j + 8 ];               
        fDummyTarget = Source[ j + 9 ];               
        fDummyTarget = Source[ j + 10 ];               
        fDummyTarget = Source[ j + 11 ];               
        fDummyTarget = Source[ j + 12 ];               
        fDummyTarget = Source[ j + 13 ];               
        fDummyTarget = Source[ j + 14 ];               
        fDummyTarget = Source[ j + 15 ];               
      }      
    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = ( 4 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Integer Read, PrefetchNTA: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth, Integer Read, PrefetchNTA: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}

double BandwidthIntegerCopy( int NumDigits )
{
  int liIterate;
  int liFill;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2000000;
  double elapsedTime;
  double memoryBandwidth;
  
  // fill the array with crap:
  for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
  {
    Source[ liFill ] = rand();
  }

  clock_t stopTime;
  clock_t startTime = clock();
  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
    {
      Target[ liFill ] = Source[ liFill ];
    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = ( 2 * 4 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Integer Copy: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth Integer Copy: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}

double BandwidthIntegerAdd( int NumDigits )
{
  int liIterate;
  int liFill;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2000000;
  double elapsedTime;
  double memoryBandwidth;
  
  // fill the array with crap:
  for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
  {
    Source[ liFill ] = rand();
    SourceB[ liFill ] = -rand();
  }

  clock_t stopTime;
  clock_t startTime = clock();
  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
    {
      Target[ liFill ] = Source[ liFill ] + SourceB[ liFill ];
    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = ( 3 * 4 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Integer Add: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth Integer Add: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}

double BandwidthIntegerScale( int NumDigits )
{
  int liIterate;
  int liFill;
  int liScale = -1;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2000000;
  double elapsedTime;
  double memoryBandwidth;
  
  // fill the array with crap:
  for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
  {
    Source[ liFill ] = rand();
  }

  clock_t stopTime;
  clock_t startTime = clock();
  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
    {
      Target[ liFill ] = liScale * Source[ liFill ];
    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = 2 * ( 4 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Integer Scale: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth Integer Scale: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}

double BandwidthIntegerTriad( int NumDigits )
{
  int liIterate;
  int liFill;
  int liScale = 3;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2000000;
  double elapsedTime;
  double memoryBandwidth;
  
  // fill the source array with crap:
  for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
  {
    Source[ liFill ] = rand() / 4;
    SourceB[ liFill ] = rand() / 3;
  }

  clock_t stopTime;
  clock_t startTime = clock();
  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
    {
      Target[ liFill ] = liScale * Source[ liFill ] + SourceB[ liFill ];
    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = ( 3 * 4 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Integer Triad: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth Integer Triad: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}

double BandwidthDoubleRead( int NumDigits )
{
  int liIterate;
  int liFill;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2000000;
  double elapsedTime;
  double memoryBandwidth;
  
  // fill the array with crap:
  for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
  {
    doubleTarget[ liFill ] = rand() * 1.1;
  }

  clock_t stopTime;
  clock_t startTime = clock();
  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
    {
      fdoubleDummyTarget = doubleTarget[ liFill ];
    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = ( 8 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Double Read: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth Double Read: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}

double BandwidthDoubleReadReverse( int NumDigits )
{
  int liIterate;
  int liFill;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2000000;
  double elapsedTime;
  double memoryBandwidth;
  
  // fill the array with crap:
  for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
  {
    doubleTarget[ liFill ] = rand() * 1.1;
  }

  clock_t stopTime;
  clock_t startTime = clock();
  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    for ( liFill = NUMBER_OF_ARRAY_ELEMENTS - 1; liFill >= 0; liFill-- )
    {
      fdoubleDummyTarget = doubleTarget[ liFill ];
    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = ( 8 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Double Read: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth Double Read: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}

double BandwidthDoubleWrite( int NumDigits )
{
  int liIterate;
  int liFill;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2000000;
  double elapsedTime;
  double memoryBandwidth;

  clock_t stopTime;
  clock_t startTime = clock();
  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
    {
      doubleTarget[ liFill ] = (double) liIterate;
    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = ( 8 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Double Write: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth, Double Write: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}

double BandwidthDoubleCopy( int NumDigits )
{
  int liIterate;
  int liFill;
  int liDummyTarget;
  const int NUMBER_OF_ITERATIONS = 1000;
  const int NUMBER_OF_ARRAY_ELEMENTS = 2000000;
  double elapsedTime;
  double memoryBandwidth;
  
  // fill the array with crap:
  for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
  {
    doubleSource[ liFill ] = 1.1 * rand();
  }

  clock_t stopTime;
  clock_t startTime = clock();
  for ( liIterate = 1; liIterate < NUMBER_OF_ITERATIONS; liIterate++ )
  {
    for ( liFill = 0; liFill < NUMBER_OF_ARRAY_ELEMENTS; liFill++ )
    {
      doubleTarget[ liFill ] = doubleSource[ liFill ];
    }
  }
  stopTime = clock();

  elapsedTime = (stopTime - startTime) / (CLOCKS_PER_SEC / (double) 1000.0);
  memoryBandwidth = ( 2 * 8 * (double)NUMBER_OF_ARRAY_ELEMENTS * (double)NUMBER_OF_ITERATIONS ) / elapsedTime / (double) 1000.0;

  cout  << setiosflags( ios::fixed )
        << setprecision( 0 );
  cout  << "Memory Bandwidth, Double Copy: "
        << elapsedTime
        << " ms "
        << endl;
  cout  << "Memory Bandwidth, Double Copy: "
        << memoryBandwidth
        << " MB/s"
        << endl;

  return elapsedTime;
}
