// Raw memory manipulators -*- C++ -*-

// Copyright (C) 2001, 2004 Free Software Foundation, Inc.
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
 * Copyright (c) 1996,1997
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

/** @file stl_uninitialized.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef _STL_UNINITIALIZED_H
#define _STL_UNINITIALIZED_H 1

#include "cstring"

namespace std
{
  // uninitialized_copy
  template<typename _InputIterator, typename _ForwardIterator>
    inline _ForwardIterator
    __uninitialized_copy_aux(_InputIterator __first, _InputIterator __last,
			     _ForwardIterator __result,
			     __true_type)
    { return std::copy(__first, __last, __result); }

  template<typename _InputIterator, typename _ForwardIterator>
    inline _ForwardIterator
    __uninitialized_copy_aux(_InputIterator __first, _InputIterator __last,
			     _ForwardIterator __result,
			     __false_type)
    {
      _ForwardIterator __cur = __result;
      try
	{
	  for ( ; __first != __last; ++__first, ++__cur)
	    std::_Construct(&*__cur, *__first);
	  return __cur;
	}
      catch(...)
	{
	  std::_Destroy(__result, __cur);
	  __throw_exception_again;
	}
    }

  /**
   *  @brief Copies the range [first,last) into result.
   *  @param  first  An input iterator.
   *  @param  last   An input iterator.
   *  @param  result An output iterator.
   *  @return   result + (first - last)
   *
   *  Like copy(), but does not require an initialized output range.
  */
  template<typename _InputIterator, typename _ForwardIterator>
    inline _ForwardIterator
    uninitialized_copy(_InputIterator __first, _InputIterator __last,
		       _ForwardIterator __result)
    {
      typedef typename iterator_traits<_ForwardIterator>::value_type _ValueType;
      typedef typename __type_traits<_ValueType>::is_POD_type _Is_POD;
      return std::__uninitialized_copy_aux(__first, __last, __result,
					   _Is_POD());
    }

  inline char*
  uninitialized_copy(const char* __first, const char* __last, char* __result)
  {
    std::memmove(__result, __first, __last - __first);
    return __result + (__last - __first);
  }

  inline wchar_t*
  uninitialized_copy(const wchar_t* __first, const wchar_t* __last,
		     wchar_t* __result)
  {
    std::memmove(__result, __first, sizeof(wchar_t) * (__last - __first));
    return __result + (__last - __first);
  }

  // Valid if copy construction is equivalent to assignment, and if the
  // destructor is trivial.
  template<typename _ForwardIterator, typename _Tp>
    inline void
    __uninitialized_fill_aux(_ForwardIterator __first,
			     _ForwardIterator __last,
			     const _Tp& __x, __true_type)
    { std::fill(__first, __last, __x); }

  template<typename _ForwardIterator, typename _Tp>
    void
    __uninitialized_fill_aux(_ForwardIterator __first, _ForwardIterator __last,
			     const _Tp& __x, __false_type)
    {
      _ForwardIterator __cur = __first;
      try
	{
	  for ( ; __cur != __last; ++__cur)
	    std::_Construct(&*__cur, __x);
	}
      catch(...)
	{
	  std::_Destroy(__first, __cur);
	  __throw_exception_again;
	}
    }

  /**
   *  @brief Copies the value x into the range [first,last).
   *  @param  first  An input iterator.
   *  @param  last   An input iterator.
   *  @param  x      The source value.
   *  @return   Nothing.
   *
   *  Like fill(), but does not require an initialized output range.
  */
  template<typename _ForwardIterator, typename _Tp>
    inline void
    uninitialized_fill(_ForwardIterator __first, _ForwardIterator __last,
		       const _Tp& __x)
    {
      typedef typename iterator_traits<_ForwardIterator>::value_type _ValueType;
      typedef typename __type_traits<_ValueType>::is_POD_type _Is_POD;
      std::__uninitialized_fill_aux(__first, __last, __x, _Is_POD());
    }

  // Valid if copy construction is equivalent to assignment, and if the
  //  destructor is trivial.
  template<typename _ForwardIterator, typename _Size, typename _Tp>
    inline _ForwardIterator
    __uninitialized_fill_n_aux(_ForwardIterator __first, _Size __n,
			       const _Tp& __x, __true_type)
    { return std::fill_n(__first, __n, __x); }

  template<typename _ForwardIterator, typename _Size, typename _Tp>
    _ForwardIterator
    __uninitialized_fill_n_aux(_ForwardIterator __first, _Size __n,
			       const _Tp& __x, __false_type)
    {
      _ForwardIterator __cur = __first;
      try
	{
	  for ( ; __n > 0; --__n, ++__cur)
	    std::_Construct(&*__cur, __x);
	  return __cur;
	}
      catch(...)
	{
	  std::_Destroy(__first, __cur);
	  __throw_exception_again;
	}
    }

  /**
   *  @brief Copies the value x into the range [first,first+n).
   *  @param  first  An input iterator.
   *  @param  n      The number of copies to make.
   *  @param  x      The source value.
   *  @return   first+n
   *
   *  Like fill_n(), but does not require an initialized output range.
  */
  template<typename _ForwardIterator, typename _Size, typename _Tp>
    inline _ForwardIterator
    uninitialized_fill_n(_ForwardIterator __first, _Size __n, const _Tp& __x)
    {
      typedef typename iterator_traits<_ForwardIterator>::value_type _ValueType;
      typedef typename __type_traits<_ValueType>::is_POD_type _Is_POD;
      return std::__uninitialized_fill_n_aux(__first, __n, __x, _Is_POD());
    }

  // Extensions: __uninitialized_copy_copy, __uninitialized_copy_fill,
  // __uninitialized_fill_copy.

  // __uninitialized_copy_copy
  // Copies [first1, last1) into [result, result + (last1 - first1)), and
  //  copies [first2, last2) into
  //  [result, result + (last1 - first1) + (last2 - first2)).

  template<typename _InputIterator1, typename _InputIterator2,
	   typename _ForwardIterator>
    inline _ForwardIterator
    __uninitialized_copy_copy(_InputIterator1 __first1,
			      _InputIterator1 __last1,
			      _InputIterator2 __first2,
			      _InputIterator2 __last2,
			      _ForwardIterator __result)
    {
      _ForwardIterator __mid = std::uninitialized_copy(__first1, __last1,
						       __result);
      try
	{
	  return std::uninitialized_copy(__first2, __last2, __mid);
	}
      catch(...)
	{
	  std::_Destroy(__result, __mid);
	  __throw_exception_again;
	}
    }

  // __uninitialized_fill_copy
  // Fills [result, mid) with x, and copies [first, last) into
  //  [mid, mid + (last - first)).
  template<typename _ForwardIterator, typename _Tp, typename _InputIterator>
    inline _ForwardIterator
    __uninitialized_fill_copy(_ForwardIterator __result, _ForwardIterator __mid,
			      const _Tp& __x, _InputIterator __first,
			      _InputIterator __last)
    {
      std::uninitialized_fill(__result, __mid, __x);
      try
	{
	  return std::uninitialized_copy(__first, __last, __mid);
	}
      catch(...)
	{
	  std::_Destroy(__result, __mid);
	  __throw_exception_again;
	}
    }

  // __uninitialized_copy_fill
  // Copies [first1, last1) into [first2, first2 + (last1 - first1)), and
  //  fills [first2 + (last1 - first1), last2) with x.
  template<typename _InputIterator, typename _ForwardIterator, typename _Tp>
    inline void
    __uninitialized_copy_fill(_InputIterator __first1, _InputIterator __last1,
			      _ForwardIterator __first2,
			      _ForwardIterator __last2, const _Tp& __x)
    {
      _ForwardIterator __mid2 = std::uninitialized_copy(__first1, __last1,
							__first2);
      try
	{
	  std::uninitialized_fill(__mid2, __last2, __x);
	}
      catch(...)
	{
	  std::_Destroy(__first2, __mid2);
	  __throw_exception_again;
	}
    }

} // namespace std

#endif /* _STL_UNINITIALIZED_H */
