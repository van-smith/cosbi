// Bits and pieces used in algorithms -*- C++ -*-

// Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1996-1998
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

/** @file stl_algobase.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef _ALGOBASE_H
#define _ALGOBASE_H 1

#include "c++config.h"
#include "cstring"
#include "climits"
#include "cstdlib"
#include "cstddef"
#include "new"
#include "iosfwd"
#include "stl_pair.h"
#include "type_traits.h"
#include "stl_iterator_base_types.h"
#include "stl_iterator_base_funcs.h"
#include "stl_iterator.h"
#include "concept_check.h"
#include "debug.h"

namespace std
{
  /**
   *  @brief Swaps the contents of two iterators.
   *  @param  a  An iterator.
   *  @param  b  Another iterator.
   *  @return   Nothing.
   *
   *  This function swaps the values pointed to by two iterators, not the
   *  iterators themselves.
  */
  template<typename _ForwardIterator1, typename _ForwardIterator2>
    inline void
    iter_swap(_ForwardIterator1 __a, _ForwardIterator2 __b)
    {
      typedef typename iterator_traits<_ForwardIterator1>::value_type
	_ValueType1;
      typedef typename iterator_traits<_ForwardIterator2>::value_type
	_ValueType2;

      // concept requirements
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator1>)
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator2>)
      __glibcxx_function_requires(_ConvertibleConcept<_ValueType1,
				  _ValueType2>)
      __glibcxx_function_requires(_ConvertibleConcept<_ValueType2,
				  _ValueType1>)

      const _ValueType1 __tmp = *__a;
      *__a = *__b;
      *__b = __tmp;
    }

  /**
   *  @brief Swaps two values.
   *  @param  a  A thing of arbitrary type.
   *  @param  b  Another thing of arbitrary type.
   *  @return   Nothing.
   *
   *  This is the simple classic generic implementation.  It will work on
   *  any type which has a copy constructor and an assignment operator.
  */
  template<typename _Tp>
    inline void
    swap(_Tp& __a, _Tp& __b)
    {
      // concept requirements
      __glibcxx_function_requires(_SGIAssignableConcept<_Tp>)

      const _Tp __tmp = __a;
      __a = __b;
      __b = __tmp;
    }

  #undef min
  #undef max

  /**
   *  @brief This does what you think it does.
   *  @param  a  A thing of arbitrary type.
   *  @param  b  Another thing of arbitrary type.
   *  @return   The lesser of the parameters.
   *
   *  This is the simple classic generic implementation.  It will work on
   *  temporary expressions, since they are only evaluated once, unlike a
   *  preprocessor macro.
  */
  template<typename _Tp>
    inline const _Tp&
    min(const _Tp& __a, const _Tp& __b)
    {
      // concept requirements
      __glibcxx_function_requires(_LessThanComparableConcept<_Tp>)
      //return __b < __a ? __b : __a;
      if (__b < __a)
	return __b;
      return __a;
    }

  /**
   *  @brief This does what you think it does.
   *  @param  a  A thing of arbitrary type.
   *  @param  b  Another thing of arbitrary type.
   *  @return   The greater of the parameters.
   *
   *  This is the simple classic generic implementation.  It will work on
   *  temporary expressions, since they are only evaluated once, unlike a
   *  preprocessor macro.
  */
  template<typename _Tp>
    inline const _Tp&
    max(const _Tp& __a, const _Tp& __b)
    {
      // concept requirements
      __glibcxx_function_requires(_LessThanComparableConcept<_Tp>)
      //return  __a < __b ? __b : __a;
      if (__a < __b)
	return __b;
      return __a;
    }

  /**
   *  @brief This does what you think it does.
   *  @param  a  A thing of arbitrary type.
   *  @param  b  Another thing of arbitrary type.
   *  @param  comp  A @link s20_3_3_comparisons comparison functor@endlink.
   *  @return   The lesser of the parameters.
   *
   *  This will work on temporary expressions, since they are only evaluated
   *  once, unlike a preprocessor macro.
  */
  template<typename _Tp, typename _Compare>
    inline const _Tp&
    min(const _Tp& __a, const _Tp& __b, _Compare __comp)
    {
      //return __comp(__b, __a) ? __b : __a;
      if (__comp(__b, __a))
	return __b;
      return __a;
    }

  /**
   *  @brief This does what you think it does.
   *  @param  a  A thing of arbitrary type.
   *  @param  b  Another thing of arbitrary type.
   *  @param  comp  A @link s20_3_3_comparisons comparison functor@endlink.
   *  @return   The greater of the parameters.
   *
   *  This will work on temporary expressions, since they are only evaluated
   *  once, unlike a preprocessor macro.
  */
  template<typename _Tp, typename _Compare>
    inline const _Tp&
    max(const _Tp& __a, const _Tp& __b, _Compare __comp)
    {
      //return __comp(__a, __b) ? __b : __a;
      if (__comp(__a, __b))
	return __b;
      return __a;
    }

  // All of these auxiliary functions serve two purposes.  (1) Replace
  // calls to copy with memmove whenever possible.  (Memmove, not memcpy,
  // because the input and output ranges are permitted to overlap.)
  // (2) If we're using random access iterators, then write the loop as
  // a for loop with an explicit count.

  template<typename _InputIterator, typename _OutputIterator>
    inline _OutputIterator
    __copy(_InputIterator __first, _InputIterator __last,
	   _OutputIterator __result, input_iterator_tag)
    {
      for (; __first != __last; ++__result, ++__first)
	*__result = *__first;
      return __result;
    }

  template<typename _RandomAccessIterator, typename _OutputIterator>
    inline _OutputIterator
    __copy(_RandomAccessIterator __first, _RandomAccessIterator __last,
	   _OutputIterator __result, random_access_iterator_tag)
    {
      typedef typename iterator_traits<_RandomAccessIterator>::difference_type
          _Distance;
      for (_Distance __n = __last - __first; __n > 0; --__n)
	{
	  *__result = *__first;
	  ++__first;
	  ++__result;
	}
      return __result;
    }

  template<typename _Tp>
    inline _Tp*
    __copy_trivial(const _Tp* __first, const _Tp* __last, _Tp* __result)
    {
      std::memmove(__result, __first, sizeof(_Tp) * (__last - __first));
      return __result + (__last - __first);
    }

  template<typename _InputIterator, typename _OutputIterator>
    inline _OutputIterator
    __copy_aux2(_InputIterator __first, _InputIterator __last,
		_OutputIterator __result, __false_type)
    { return std::__copy(__first, __last, __result,
			 std::__iterator_category(__first)); }

  template<typename _InputIterator, typename _OutputIterator>
    inline _OutputIterator
    __copy_aux2(_InputIterator __first, _InputIterator __last,
		_OutputIterator __result, __true_type)
    { return std::__copy(__first, __last, __result,
			 std::__iterator_category(__first)); }

  template<typename _Tp>
    inline _Tp*
    __copy_aux2(_Tp* __first, _Tp* __last, _Tp* __result, __true_type)
    { return std::__copy_trivial(__first, __last, __result); }

  template<typename _Tp>
    inline _Tp*
    __copy_aux2(const _Tp* __first, const _Tp* __last, _Tp* __result,
		__true_type)
    { return std::__copy_trivial(__first, __last, __result); }

  template<typename _InputIterator, typename _OutputIterator>
    inline _OutputIterator
    __copy_ni2(_InputIterator __first, _InputIterator __last,
	       _OutputIterator __result, __true_type)
    {
      typedef typename iterator_traits<_InputIterator>::value_type
	_ValueType;
      typedef typename __type_traits<
	_ValueType>::has_trivial_assignment_operator _Trivial;
      return _OutputIterator(std::__copy_aux2(__first, __last, __result.base(),
					      _Trivial()));
    }

  template<typename _InputIterator, typename _OutputIterator>
    inline _OutputIterator
    __copy_ni2(_InputIterator __first, _InputIterator __last,
	       _OutputIterator __result, __false_type)
    {
      typedef typename iterator_traits<_InputIterator>::value_type _ValueType;
      typedef typename __type_traits<
	_ValueType>::has_trivial_assignment_operator _Trivial;
      return std::__copy_aux2(__first, __last, __result, _Trivial());
    }

  template<typename _InputIterator, typename _OutputIterator>
    inline _OutputIterator
    __copy_ni1(_InputIterator __first, _InputIterator __last,
	       _OutputIterator __result, __true_type)
    {
      typedef typename _Is_normal_iterator<_OutputIterator>::_Normal __Normal;
      return std::__copy_ni2(__first.base(), __last.base(),
			     __result, __Normal());
    }

  template<typename _InputIterator, typename _OutputIterator>
    inline _OutputIterator
    __copy_ni1(_InputIterator __first, _InputIterator __last,
	       _OutputIterator __result, __false_type)
    {
      typedef typename _Is_normal_iterator<_OutputIterator>::_Normal __Normal;
      return std::__copy_ni2(__first, __last, __result, __Normal());
    }

  /**
   *  @brief Copies the range [first,last) into result.
   *  @param  first  An input iterator.
   *  @param  last   An input iterator.
   *  @param  result An output iterator.
   *  @return   result + (first - last)
   *
   *  This inline function will boil down to a call to @c memmove whenever
   *  possible.  Failing that, if random access iterators are passed, then the
   *  loop count will be known (and therefore a candidate for compiler
   *  optimizations such as unrolling).  Result may not be contained within
   *  [first,last); the copy_backward function should be used instead.
   *
   *  Note that the end of the output range is permitted to be contained
   *  within [first,last).
  */
  template<typename _InputIterator, typename _OutputIterator>
    inline _OutputIterator
    copy(_InputIterator __first, _InputIterator __last,
	 _OutputIterator __result)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
      __glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,
	    typename iterator_traits<_InputIterator>::value_type>)
      __glibcxx_requires_valid_range(__first, __last);

       typedef typename _Is_normal_iterator<_InputIterator>::_Normal __Normal;
       return std::__copy_ni1(__first, __last, __result, __Normal());
    }

  template<typename _BidirectionalIterator1, typename _BidirectionalIterator2>
    inline _BidirectionalIterator2
    __copy_backward(_BidirectionalIterator1 __first,
		    _BidirectionalIterator1 __last,
		    _BidirectionalIterator2 __result,
		    bidirectional_iterator_tag)
    {
      while (__first != __last)
        *--__result = *--__last;
      return __result;
    }

  template<typename _RandomAccessIterator, typename _BidirectionalIterator>
    inline _BidirectionalIterator
    __copy_backward(_RandomAccessIterator __first, _RandomAccessIterator __last,
		    _BidirectionalIterator __result, random_access_iterator_tag)
    {
      typename iterator_traits<_RandomAccessIterator>::difference_type __n;
      for (__n = __last - __first; __n > 0; --__n)
        *--__result = *--__last;
      return __result;
    }


  // This dispatch class is a workaround for compilers that do not
  // have partial ordering of function templates.  All we're doing is
  // creating a specialization so that we can turn a call to copy_backward
  // into a memmove whenever possible.
  template<typename _BidirectionalIterator1, typename _BidirectionalIterator2,
           typename _BoolType>
    struct __copy_backward_dispatch
    {
      static _BidirectionalIterator2
      copy(_BidirectionalIterator1 __first, _BidirectionalIterator1 __last,
	   _BidirectionalIterator2 __result)
      { return std::__copy_backward(__first, __last, __result,
				    std::__iterator_category(__first)); }
    };

  template<typename _Tp>
    struct __copy_backward_dispatch<_Tp*, _Tp*, __true_type>
    {
      static _Tp*
      copy(const _Tp* __first, const _Tp* __last, _Tp* __result)
      {
	const ptrdiff_t _Num = __last - __first;
	std::memmove(__result - _Num, __first, sizeof(_Tp) * _Num);
	return __result - _Num;
      }
    };

  template<typename _Tp>
    struct __copy_backward_dispatch<const _Tp*, _Tp*, __true_type>
    {
      static _Tp*
      copy(const _Tp* __first, const _Tp* __last, _Tp* __result)
      {
	return  std::__copy_backward_dispatch<_Tp*, _Tp*, __true_type>
	  ::copy(__first, __last, __result);
      }
    };

  template<typename _BI1, typename _BI2>
    inline _BI2
    __copy_backward_aux(_BI1 __first, _BI1 __last, _BI2 __result)
    {
      typedef typename __type_traits<typename iterator_traits<_BI2>::value_type>
			    ::has_trivial_assignment_operator _Trivial;
      return
	std::__copy_backward_dispatch<_BI1, _BI2, _Trivial>::copy(__first,
								  __last,
								  __result);
    }

  template <typename _BI1, typename _BI2>
    inline _BI2
    __copy_backward_output_normal_iterator(_BI1 __first, _BI1 __last,
					   _BI2 __result, __true_type)
    { return _BI2(std::__copy_backward_aux(__first, __last, __result.base())); }

  template <typename _BI1, typename _BI2>
    inline _BI2
    __copy_backward_output_normal_iterator(_BI1 __first, _BI1 __last,
					   _BI2 __result, __false_type)
    { return std::__copy_backward_aux(__first, __last, __result); }

  template <typename _BI1, typename _BI2>
    inline _BI2
    __copy_backward_input_normal_iterator(_BI1 __first, _BI1 __last,
					  _BI2 __result, __true_type)
    {
      typedef typename _Is_normal_iterator<_BI2>::_Normal __Normal;
      return std::__copy_backward_output_normal_iterator(__first.base(),
							 __last.base(),
							 __result, __Normal());
    }

  template <typename _BI1, typename _BI2>
    inline _BI2
    __copy_backward_input_normal_iterator(_BI1 __first, _BI1 __last,
					  _BI2 __result, __false_type)
    {
      typedef typename _Is_normal_iterator<_BI2>::_Normal __Normal;
      return std::__copy_backward_output_normal_iterator(__first, __last,
							 __result, __Normal());
    }

  /**
   *  @brief Copies the range [first,last) into result.
   *  @param  first  A bidirectional iterator.
   *  @param  last   A bidirectional iterator.
   *  @param  result A bidirectional iterator.
   *  @return   result - (first - last)
   *
   *  The function has the same effect as copy, but starts at the end of the
   *  range and works its way to the start, returning the start of the result.
   *  This inline function will boil down to a call to @c memmove whenever
   *  possible.  Failing that, if random access iterators are passed, then the
   *  loop count will be known (and therefore a candidate for compiler
   *  optimizations such as unrolling).
   *
   *  Result may not be in the range [first,last).  Use copy instead.  Note
   *  that the start of the output range may overlap [first,last).
  */
  template <typename _BI1, typename _BI2>
    inline _BI2
    copy_backward(_BI1 __first, _BI1 __last, _BI2 __result)
    {
      // concept requirements
      __glibcxx_function_requires(_BidirectionalIteratorConcept<_BI1>)
      __glibcxx_function_requires(_Mutable_BidirectionalIteratorConcept<_BI2>)
      __glibcxx_function_requires(_ConvertibleConcept<
	    typename iterator_traits<_BI1>::value_type,
	    typename iterator_traits<_BI2>::value_type>)
      __glibcxx_requires_valid_range(__first, __last);

      typedef typename _Is_normal_iterator<_BI1>::_Normal __Normal;
      return std::__copy_backward_input_normal_iterator(__first, __last,
							__result, __Normal());
    }


  /**
   *  @brief Fills the range [first,last) with copies of value.
   *  @param  first  A forward iterator.
   *  @param  last   A forward iterator.
   *  @param  value  A reference-to-const of arbitrary type.
   *  @return   Nothing.
   *
   *  This function fills a range with copies of the same value.  For one-byte
   *  types filling contiguous areas of memory, this becomes an inline call to
   *  @c memset.
  */
  template<typename _ForwardIterator, typename _Tp>
    void
    fill(_ForwardIterator __first, _ForwardIterator __last, const _Tp& __value)
    {
      // concept requirements
      __glibcxx_function_requires(_Mutable_ForwardIteratorConcept<
				  _ForwardIterator>)
      __glibcxx_requires_valid_range(__first, __last);

      for ( ; __first != __last; ++__first)
	*__first = __value;
    }

  /**
   *  @brief Fills the range [first,first+n) with copies of value.
   *  @param  first  An output iterator.
   *  @param  n      The count of copies to perform.
   *  @param  value  A reference-to-const of arbitrary type.
   *  @return   The iterator at first+n.
   *
   *  This function fills a range with copies of the same value.  For one-byte
   *  types filling contiguous areas of memory, this becomes an inline call to
   *  @c memset.
  */
  template<typename _OutputIterator, typename _Size, typename _Tp>
    _OutputIterator
    fill_n(_OutputIterator __first, _Size __n, const _Tp& __value)
    {
      // concept requirements
      __glibcxx_function_requires(_OutputIteratorConcept<_OutputIterator,_Tp>)

      for ( ; __n > 0; --__n, ++__first)
	*__first = __value;
      return __first;
    }

  // Specialization: for one-byte types we can use memset.
  inline void
  fill(unsigned char* __first, unsigned char* __last, const unsigned char& __c)
  {
    __glibcxx_requires_valid_range(__first, __last);
    const unsigned char __tmp = __c;
    std::memset(__first, __tmp, __last - __first);
  }

  inline void
  fill(signed char* __first, signed char* __last, const signed char& __c)
  {
    __glibcxx_requires_valid_range(__first, __last);
    const signed char __tmp = __c;
    std::memset(__first, static_cast<unsigned char>(__tmp), __last - __first);
  }

  inline void
  fill(char* __first, char* __last, const char& __c)
  {
    __glibcxx_requires_valid_range(__first, __last);
    const char __tmp = __c;
    std::memset(__first, static_cast<unsigned char>(__tmp), __last - __first);
  }

  template<typename _Size>
    inline unsigned char*
    fill_n(unsigned char* __first, _Size __n, const unsigned char& __c)
    {
      std::fill(__first, __first + __n, __c);
      return __first + __n;
    }

  template<typename _Size>
    inline signed char*
    fill_n(char* __first, _Size __n, const signed char& __c)
    {
      std::fill(__first, __first + __n, __c);
      return __first + __n;
    }

  template<typename _Size>
    inline char*
    fill_n(char* __first, _Size __n, const char& __c)
    {
      std::fill(__first, __first + __n, __c);
      return __first + __n;
    }


  /**
   *  @brief Finds the places in ranges which don't match.
   *  @param  first1  An input iterator.
   *  @param  last1   An input iterator.
   *  @param  first2  An input iterator.
   *  @return   A pair of iterators pointing to the first mismatch.
   *
   *  This compares the elements of two ranges using @c == and returns a pair
   *  of iterators.  The first iterator points into the first range, the
   *  second iterator points into the second range, and the elements pointed
   *  to by the iterators are not equal.
  */
  template<typename _InputIterator1, typename _InputIterator2>
    pair<_InputIterator1, _InputIterator2>
    mismatch(_InputIterator1 __first1, _InputIterator1 __last1,
	     _InputIterator2 __first2)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_function_requires(_EqualityComparableConcept<
	    typename iterator_traits<_InputIterator1>::value_type>)
      __glibcxx_function_requires(_EqualityComparableConcept<
	    typename iterator_traits<_InputIterator2>::value_type>)
      __glibcxx_requires_valid_range(__first1, __last1);

      while (__first1 != __last1 && *__first1 == *__first2)
        {
	  ++__first1;
	  ++__first2;
        }
      return pair<_InputIterator1, _InputIterator2>(__first1, __first2);
    }

  /**
   *  @brief Finds the places in ranges which don't match.
   *  @param  first1  An input iterator.
   *  @param  last1   An input iterator.
   *  @param  first2  An input iterator.
   *  @param  binary_pred  A binary predicate @link s20_3_1_base functor@endlink.
   *  @return   A pair of iterators pointing to the first mismatch.
   *
   *  This compares the elements of two ranges using the binary_pred
   *  parameter, and returns a pair
   *  of iterators.  The first iterator points into the first range, the
   *  second iterator points into the second range, and the elements pointed
   *  to by the iterators are not equal.
  */
  template<typename _InputIterator1, typename _InputIterator2,
	   typename _BinaryPredicate>
    pair<_InputIterator1, _InputIterator2>
    mismatch(_InputIterator1 __first1, _InputIterator1 __last1,
	     _InputIterator2 __first2, _BinaryPredicate __binary_pred)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_requires_valid_range(__first1, __last1);

      while (__first1 != __last1 && __binary_pred(*__first1, *__first2))
        {
	  ++__first1;
	  ++__first2;
        }
      return pair<_InputIterator1, _InputIterator2>(__first1, __first2);
    }

  /**
   *  @brief Tests a range for element-wise equality.
   *  @param  first1  An input iterator.
   *  @param  last1   An input iterator.
   *  @param  first2  An input iterator.
   *  @return   A boolean true or false.
   *
   *  This compares the elements of two ranges using @c == and returns true or
   *  false depending on whether all of the corresponding elements of the
   *  ranges are equal.
  */
  template<typename _InputIterator1, typename _InputIterator2>
    inline bool
    equal(_InputIterator1 __first1, _InputIterator1 __last1,
	  _InputIterator2 __first2)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_function_requires(_EqualOpConcept<
	    typename iterator_traits<_InputIterator1>::value_type,
	    typename iterator_traits<_InputIterator2>::value_type>)
      __glibcxx_requires_valid_range(__first1, __last1);

      for ( ; __first1 != __last1; ++__first1, ++__first2)
	if (!(*__first1 == *__first2))
	  return false;
      return true;
    }

  /**
   *  @brief Tests a range for element-wise equality.
   *  @param  first1  An input iterator.
   *  @param  last1   An input iterator.
   *  @param  first2  An input iterator.
   *  @param  binary_pred  A binary predicate @link s20_3_1_base functor@endlink.
   *  @return   A boolean true or false.
   *
   *  This compares the elements of two ranges using the binary_pred
   *  parameter, and returns true or
   *  false depending on whether all of the corresponding elements of the
   *  ranges are equal.
  */
  template<typename _InputIterator1, typename _InputIterator2,
	   typename _BinaryPredicate>
    inline bool
    equal(_InputIterator1 __first1, _InputIterator1 __last1,
	  _InputIterator2 __first2,
	  _BinaryPredicate __binary_pred)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_requires_valid_range(__first1, __last1);

      for ( ; __first1 != __last1; ++__first1, ++__first2)
	if (!__binary_pred(*__first1, *__first2))
	  return false;
      return true;
    }

  /**
   *  @brief Performs "dictionary" comparison on ranges.
   *  @param  first1  An input iterator.
   *  @param  last1   An input iterator.
   *  @param  first2  An input iterator.
   *  @param  last2   An input iterator.
   *  @return   A boolean true or false.
   *
   *  "Returns true if the sequence of elements defined by the range
   *  [first1,last1) is lexicographically less than the sequence of elements
   *  defined by the range [first2,last2).  Returns false otherwise."
   *  (Quoted from [25.3.8]/1.)  If the iterators are all character pointers,
   *  then this is an inline call to @c memcmp.
  */
  template<typename _InputIterator1, typename _InputIterator2>
    bool
    lexicographical_compare(_InputIterator1 __first1, _InputIterator1 __last1,
			    _InputIterator2 __first2, _InputIterator2 __last2)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_function_requires(_LessThanComparableConcept<
	    typename iterator_traits<_InputIterator1>::value_type>)
      __glibcxx_function_requires(_LessThanComparableConcept<
	    typename iterator_traits<_InputIterator2>::value_type>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      for (;__first1 != __last1 && __first2 != __last2; ++__first1, ++__first2)
	{
	  if (*__first1 < *__first2)
	    return true;
	  if (*__first2 < *__first1)
	    return false;
	}
      return __first1 == __last1 && __first2 != __last2;
    }

  /**
   *  @brief Performs "dictionary" comparison on ranges.
   *  @param  first1  An input iterator.
   *  @param  last1   An input iterator.
   *  @param  first2  An input iterator.
   *  @param  last2   An input iterator.
   *  @param  comp  A @link s20_3_3_comparisons comparison functor@endlink.
   *  @return   A boolean true or false.
   *
   *  The same as the four-parameter @c lexigraphical_compare, but uses the
   *  comp parameter instead of @c <.
  */
  template<typename _InputIterator1, typename _InputIterator2,
	   typename _Compare>
    bool
    lexicographical_compare(_InputIterator1 __first1, _InputIterator1 __last1,
			    _InputIterator2 __first2, _InputIterator2 __last2,
			    _Compare __comp)
    {
      // concept requirements
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator1>)
      __glibcxx_function_requires(_InputIteratorConcept<_InputIterator2>)
      __glibcxx_requires_valid_range(__first1, __last1);
      __glibcxx_requires_valid_range(__first2, __last2);

      for ( ; __first1 != __last1 && __first2 != __last2
	    ; ++__first1, ++__first2)
	{
	  if (__comp(*__first1, *__first2))
	    return true;
	  if (__comp(*__first2, *__first1))
	    return false;
	}
      return __first1 == __last1 && __first2 != __last2;
    }

  inline bool
  lexicographical_compare(const unsigned char* __first1,
			  const unsigned char* __last1,
			  const unsigned char* __first2,
			  const unsigned char* __last2)
  {
    __glibcxx_requires_valid_range(__first1, __last1);
    __glibcxx_requires_valid_range(__first2, __last2);

    const size_t __len1 = __last1 - __first1;
    const size_t __len2 = __last2 - __first2;
    const int __result = std::memcmp(__first1, __first2,
				     std::min(__len1, __len2));
    return __result != 0 ? __result < 0 : __len1 < __len2;
  }

  inline bool
  lexicographical_compare(const char* __first1, const char* __last1,
			  const char* __first2, const char* __last2)
  {
    __glibcxx_requires_valid_range(__first1, __last1);
    __glibcxx_requires_valid_range(__first2, __last2);

#if CHAR_MAX == SCHAR_MAX
    return std::lexicographical_compare((const signed char*) __first1,
					(const signed char*) __last1,
					(const signed char*) __first2,
					(const signed char*) __last2);
#else /* CHAR_MAX == SCHAR_MAX */
    return std::lexicographical_compare((const unsigned char*) __first1,
					(const unsigned char*) __last1,
					(const unsigned char*) __first2,
					(const unsigned char*) __last2);
#endif /* CHAR_MAX == SCHAR_MAX */
  }

} // namespace std

#endif
