import sys
from xml.etree import ElementTree


def parse_xbrl(file_path, filing_type="4"):
  if filing_type == "4":
    ret = parse_xbrl_form4(file_path)
    if ret is not None:
        return ret
    else:
        return []


def parse_xbrl_form4(file_path):
  with open(file_path) as file:
    text = file.read()

  xml = strip_non_xml(text)
  try:
    tree = ElementTree.fromstring(xml)
  except:
      print("ERROR DUE TO STRIPPED XML for " + file_path + ": " + xml + "\nORIGINAL XML: " + text, file=sys.stderr)
      return

  transactions = []

  report_period = get_text_xpath(tree, ".//periodOfReport")
  symbol = get_text_xpath(tree, ".//issuerTradingSymbol")
  is_director = get_text_xpath(tree, ".//isDirector")
  is_officer = get_text_xpath(tree, ".//isOfficer")
  is_ten_percent_owner = get_text_xpath(tree, ".//isTenPercentOwner")
  is_other = get_text_xpath(tree, ".//isOther")
  officer_title = get_text_xpath(tree, ".//officerTitle")

  for transaction in tree.findall(".//nonDerivativeTransaction"):
    transaction_date = get_text_xpath(transaction, ".//transactionDate",
                                      nested_value=True)
    transaction_share_count = get_text_xpath(transaction, ".//transactionShares",
                                             nested_value=True, value_type=int)
    transaction_price_per_share = get_text_xpath(transaction, ".//transactionPricePerShare",
                                                 nested_value=True, value_type=float)
    buy_or_sell = get_text_xpath(transaction, ".//transactionAcquiredDisposedCode",
                                 nested_value=True)
    shares_owned_following_transaction = get_text_xpath(transaction, ".//sharesOwnedFollowingTransaction",
                                                        nested_value=True)
    direct_ownership = get_text_xpath(transaction, ".//directOrIndirectOwnership",
                                      nested_value=True)

    trans_dict = {
      'report_period': report_period,
      'symbol': symbol,
      'is_director': is_director,
      'is_officer': is_officer,
      'is_ten_percent_owner': is_ten_percent_owner,
      'is_other': is_other,
      'officer_title': officer_title,
      'transaction_date': transaction_date,
      'transaction_share_count': transaction_share_count,
      'transaction_price_per_share': transaction_price_per_share,
      'buy_or_sell': buy_or_sell,
      'shares_owned_following_transaction': shares_owned_following_transaction,
      'direct_ownership': direct_ownership
    }

    transactions.append(trans_dict)

  return transactions


def get_text_xpath(tree, target_path, nested_value=False, value_type=None):
    try:
        elem = tree.find(target_path)
        if elem is not None:
            if nested_value:
                text = elem.find(".//value").text
            else:
                text = elem.text

            if value_type is not None:
                try:
                    return value_type(text)
                except:
                    return text
            else:
                return text
    except:
        return ""


def strip_non_xml(xbrl):
  """
  Extracts only the valid XML from an XBRL string by removing the wrapping tags
  """

  # Remove XBRL header
  xml_header_index = xbrl.find('<?xml version="1.0"?>')
  xml_plus_xbrl_footer = xbrl[xml_header_index:]
  # Remove XBRL footer (closing tags)
  xbrl_footer_index = xml_plus_xbrl_footer.find('</XML>')
  return xml_plus_xbrl_footer[:xbrl_footer_index]
