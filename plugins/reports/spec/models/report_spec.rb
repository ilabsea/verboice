require 'spec_helper'
describe Report do
	let(:super_user) { Account.make role: Account::ADMIN }
  let(:user) { Account.make role: Account::USER }

  it { should belong_to(:call_log) }
end