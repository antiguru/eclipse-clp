// BEGIN LICENSE BLOCK
// Version: CMPL 1.1
//
// The contents of this file are subject to the Cisco-style Mozilla Public
// License Version 1.1 (the "License"); you may not use this file except
// in compliance with the License.  You may obtain a copy of the License
// at www.eclipse-clp.org/license.
// 
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
// the License for the specific language governing rights and limitations
// under the License. 
// 
// The Original Code is  The ECLiPSe Constraint Logic Programming System. 
// The Initial Developer of the Original Code is  Cisco Systems, Inc. 
// Portions created by the Initial Developer are
// Copyright (C) 2000 - 2006 Cisco Systems, Inc.  All Rights Reserved.
// 
// Contributor(s): Stefano Novello / Josh Singer, Parc Technologies
// 
// END LICENSE BLOCK

//Title:        Java/ECLiPSe interface
//Version:      $Id: EclipseException.java,v 1.1 2006/09/23 01:54:09 snovello Exp $
//Author:       Josh Singer / Stefano Novello
//Company:      Parc Technologies
//Description:  Superclass of ECLiPSe-related exceptions.

package com.parctechnologies.eclipse;

/**
 * The superclass of exceptions relating to the execution of the ECLiPSe engine.
 * @see Fail
 * @see Throw
 */
public class EclipseException extends Exception
{
  /**
  * Construct an EclipseException without any message.
  */
  public EclipseException()
  {
    super();
  }

  /**
   * Construct an EclipseException with a given String as its message.
   */
  public EclipseException(String s)
  {
    super(s);
  }
}
