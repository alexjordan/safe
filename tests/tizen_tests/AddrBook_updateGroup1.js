/*******************************************************************************
    Copyright (c) 2013, S-Core.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/


var contactRef = new tizen.ContactRef("1", "2");
var addressBook = tizen.contact.getAddressBook(contactRef.addressBookId);
var groups = addressBook.getGroups();
groups[0].name = 'Friends';
var __result1 = addressBook.updateGroup(groups[0]);
var __expect1 = undefined;

