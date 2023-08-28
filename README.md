# re-notion

A repo (and ultimately package) for interaction with the Notion API

So I wanted to create some connections between Google Calendar and Notion and after faffing with Zapier for a bit realised that what I wanted to do would end up expensive. Then I remembered hey Notion has an API. Then I remembered oh boo my main programming language is R. Then I remember I could bring the two together.

This will end up as a suite of tools for creating, updating, removing, and extracting data from Notion and bring it all into R.

Current functions:

`extract_database_from_url` - The URL of the database, and the one passed by the API are different, with this function you can send the URL and it'll turn it into the right Database ID

`get_database` - Gets the properties of the database.

`get_database_all_pages` - Gets a list, as a character vector, of the `page_id` for every page in a data.

`get_page` - Gets the properties of a page

`delete_page` - Deletes a page

`restore_page` - Restores a page

`page_data_to_df` - With a `page_list` from the `get_page` function, converts the data into a tibble

`extract_rich_text` - helper function to parse rich_text

`extract_title` - helper function to parse title

`extract_date` - helper function to parse date

`get_page_df` - With a `page_id` returns the page data as a single-row tibble

`get_database_df` - With a `db_id` returns a tibble of all the data (using `get_page_df`)
